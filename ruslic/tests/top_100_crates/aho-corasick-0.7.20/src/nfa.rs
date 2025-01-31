use std::cmp;
use std::collections::{BTreeSet, VecDeque};
use std::fmt;
use std::mem::size_of;
use std::ops::{Index, IndexMut};

use crate::ahocorasick::MatchKind;
use crate::automaton::Automaton;
use crate::classes::{ByteClassBuilder, ByteClasses};
use crate::error::Result;
use crate::prefilter::{self, opposite_ascii_case, Prefilter, PrefilterObj};
use crate::state_id::{dead_id, fail_id, usize_to_state_id, StateID};
use crate::Match;

/// The identifier for a pattern, which is simply the position of the pattern
/// in the sequence of patterns given by the caller.
pub type PatternID = usize;

/// The length of a pattern, in bytes.
pub type PatternLength = usize;

/// An Aho-Corasick automaton, represented as an NFA.
///
/// This is the classical formulation of Aho-Corasick, which involves building
/// up a prefix trie of a given set of patterns, and then wiring up failure
/// transitions between states in order to guarantee linear time matching. The
/// standard formulation is, technically, an NFA because of these failure
/// transitions. That is, one can see them as enabling the automaton to be in
/// multiple states at once. Indeed, during search, it is possible to check
/// the transitions on multiple states for a single input byte.
///
/// This particular implementation not only supports the standard style of
/// matching, but also provides a mode for choosing leftmost-first or
/// leftmost-longest match semantics. When a leftmost mode is chosen, some
/// failure transitions that would otherwise be added are elided. See
/// the documentation of `MatchKind` for more details and examples on how the
/// match semantics may differ.
///
/// If one wants a DFA, then it is necessary to first build an NFA and convert
/// it into a DFA. Note, however, that because we've constrained ourselves to
/// matching literal patterns, this does not need to use subset construction
/// for determinization. Instead, the DFA has at most a number of states
/// equivalent to the number of NFA states. The only real difference between
/// them is that all failure transitions are followed and pre-computed. This
/// uses much more memory, but also executes searches more quickly.
#[derive(Clone)]
pub struct NFA<S> {
    /// The match semantics built into this NFA.
    match_kind: MatchKind,
    /// The start state id as an index into `states`.
    start_id: S,
    /// The length, in bytes, of the longest pattern in this automaton. This
    /// information is useful for keeping correct buffer sizes when searching
    /// on streams.
    max_pattern_len: usize,
    /// The total number of patterns added to this automaton, including
    /// patterns that may never be matched.
    pattern_count: usize,
    /// The number of bytes of heap used by this NFA's transition table.
    heap_bytes: usize,
    /// A prefilter for quickly skipping to candidate matches, if pertinent.
    prefilter: Option<PrefilterObj>,
    /// Whether this automaton anchors all matches to the start of input.
    anchored: bool,
    /// A set of equivalence classes in terms of bytes. We compute this while
    /// building the NFA, but don't use it in the NFA's states. Instead, we
    /// use this for building the DFA. We store it on the NFA since it's easy
    /// to compute while visiting the patterns.
    byte_classes: ByteClasses,
    /// A set of states. Each state defines its own transitions, a fail
    /// transition and a set of indices corresponding to matches.
    ///
    /// The first state is always the fail state, which is used only as a
    /// sentinel. Namely, in the final NFA, no transition into the fail state
    /// exists. (Well, they do, but they aren't followed. Instead, the state's
    /// failure transition is followed.)
    ///
    /// The second state (index 1) is always the dead state. Dead states are
    /// in every automaton, but only used when leftmost-{first,longest} match
    /// semantics are enabled. Specifically, they instruct search to stop
    /// at specific points in order to report the correct match location. In
    /// the standard Aho-Corasick construction, there are no transitions to
    /// the dead state.
    ///
    /// The third state (index 2) is generally intended to be the starting or
    /// "root" state.
    states: Vec<State<S>>,
}

impl<S: StateID> NFA<S> {
    /// Returns the equivalence classes of bytes found while constructing
    /// this NFA.
    ///
    /// Note that the NFA doesn't actually make use of these equivalence
    /// classes. Instead, these are useful for building the DFA when desired.
    pub fn byte_classes(&self) -> &ByteClasses {
        &self.byte_classes
    }

    /// Returns a prefilter, if one exists.
    pub fn prefilter_obj(&self) -> Option<&PrefilterObj> {
        self.prefilter.as_ref()
    }

    /// Returns the total number of heap bytes used by this NFA's transition
    /// table.
    pub fn heap_bytes(&self) -> usize {
        self.heap_bytes
            + self.prefilter.as_ref().map_or(0, |p| p.as_ref().heap_bytes())
    }

    /// Return the length of the longest pattern in this automaton.
    pub fn max_pattern_len(&self) -> usize {
        self.max_pattern_len
    }

    /// Return the total number of patterns added to this automaton.
    pub fn pattern_count(&self) -> usize {
        self.pattern_count
    }

    /// Returns the total number of states in this NFA.
    pub fn state_len(&self) -> usize {
        self.states.len()
    }

    /// Returns the matches for the given state.
    pub fn matches(&self, id: S) -> &[(PatternID, PatternLength)] {
        &self.states[id.to_usize()].matches
    }

    /// Returns an iterator over all transitions in the given state according
    /// to the given equivalence classes, including transitions to `fail_id()`.
    /// The number of transitions returned is always equivalent to the number
    /// of equivalence classes.
    pub fn iter_all_transitions<F: FnMut(u8, S)>(
        &self,
        byte_classes: &ByteClasses,
        id: S,
        f: F,
    ) {
        self.states[id.to_usize()].trans.iter_all(byte_classes, f);
    }

    /// Returns the failure transition for the given state.
    pub fn failure_transition(&self, id: S) -> S {
        self.states[id.to_usize()].fail
    }

    /// Returns the next state for the given state and input byte.
    ///
    /// Note that this does not follow failure transitions. As such, the id
    /// returned may be `fail_id`.
    pub fn next_state(&self, current: S, input: u8) -> S {
        self.states[current.to_usize()].next_state(input)
    }

    fn state(&self, id: S) -> &State<S> {
        &self.states[id.to_usize()]
    }

    fn state_mut(&mut self, id: S) -> &mut State<S> {
        &mut self.states[id.to_usize()]
    }

    fn start(&self) -> &State<S> {
        self.state(self.start_id)
    }

    fn start_mut(&mut self) -> &mut State<S> {
        let id = self.start_id;
        self.state_mut(id)
    }

    fn iter_transitions_mut(&mut self, id: S) -> IterTransitionsMut<'_, S> {
        IterTransitionsMut::new(self, id)
    }

    fn copy_matches(&mut self, src: S, dst: S) {
        let (src, dst) =
            get_two_mut(&mut self.states, src.to_usize(), dst.to_usize());
        dst.matches.extend_from_slice(&src.matches);
    }

    fn copy_empty_matches(&mut self, dst: S) {
        let start_id = self.start_id;
        self.copy_matches(start_id, dst);
    }

    fn add_dense_state(&mut self, depth: usize) -> Result<S> {
        let trans = Transitions::Dense(Dense::new());
        let id = usize_to_state_id(self.states.len())?;
        self.states.push(State {
            trans,
            // Anchored automatons do not have any failure transitions.
            fail: if self.anchored { dead_id() } else { self.start_id },
            depth,
            matches: vec![],
        });
        Ok(id)
    }

    fn add_sparse_state(&mut self, depth: usize) -> Result<S> {
        let trans = Transitions::Sparse(vec![]);
        let id = usize_to_state_id(self.states.len())?;
        self.states.push(State {
            trans,
            // Anchored automatons do not have any failure transitions.
            fail: if self.anchored { dead_id() } else { self.start_id },
            depth,
            matches: vec![],
        });
        Ok(id)
    }
}

impl<S: StateID> Automaton for NFA<S> {
    type ID = S;

    fn match_kind(&self) -> &MatchKind {
        &self.match_kind
    }

    fn anchored(&self) -> bool {
        self.anchored
    }

    fn prefilter(&self) -> Option<&dyn Prefilter> {
        self.prefilter.as_ref().map(|p| p.as_ref())
    }

    fn start_state(&self) -> S {
        self.start_id
    }

    fn is_valid(&self, id: S) -> bool {
        id.to_usize() < self.states.len()
    }

    fn is_match_state(&self, id: S) -> bool {
        self.states[id.to_usize()].is_match()
    }

    fn get_match(
        &self,
        id: S,
        match_index: usize,
        end: usize,
    ) -> Option<Match> {
        let state = match self.states.get(id.to_usize()) {
            None => return None,
            Some(state) => state,
        };
        state.matches.get(match_index).map(|&(id, len)| Match {
            pattern: id,
            len,
            end,
        })
    }

    fn match_count(&self, id: S) -> usize {
        self.states[id.to_usize()].matches.len()
    }

    fn next_state(&self, mut current: S, input: u8) -> S {
        // This terminates since:
        //
        // 1. `State.fail` never points to fail_id().
        // 2. All `State.fail` values point to a state closer to `start`.
        // 3. The start state has no transitions to fail_id().
        loop {
            let state = &self.states[current.to_usize()];
            let next = state.next_state(input);
            if next != fail_id() {
                return next;
            }
            current = state.fail;
        }
    }
}

/// A representation of an NFA state for an Aho-Corasick automaton.
///
/// It contains the transitions to the next state, a failure transition for
/// cases where there exists no other transition for the current input byte,
/// the matches implied by visiting this state (if any) and the depth of this
/// state. The depth of a state is simply the distance from it to the start
/// state in the automaton, where the depth of the start state is 0.
#[derive(Clone, Debug)]
pub struct State<S> {
    trans: Transitions<S>,
    fail: S,
    matches: Vec<(PatternID, PatternLength)>,
    // TODO: Strictly speaking, this isn't needed for searching. It's only
    // used when building an NFA that supports leftmost match semantics. We
    // could drop this from the state and dynamically build a map only when
    // computing failure transitions, but it's not clear which is better.
    // Benchmark this.
    depth: usize,
}

impl<S: StateID> State<S> {
    fn heap_bytes(&self) -> usize {
        self.trans.heap_bytes()
            + (self.matches.len() * size_of::<(PatternID, PatternLength)>())
    }

    fn add_match(&mut self, i: PatternID, len: PatternLength) {
        self.matches.push((i, len));
    }

    fn is_match(&self) -> bool {
        !self.matches.is_empty()
    }

    fn next_state(&self, input: u8) -> S {
        self.trans.next_state(input)
    }

    fn set_next_state(&mut self, input: u8, next: S) {
        self.trans.set_next_state(input, next);
    }
}

/// Represents the transitions for a single dense state.
///
/// The primary purpose here is to encapsulate index access. Namely, since a
/// dense representation always contains 256 elements, all values of `u8` are
/// valid indices.
#[derive(Clone, Debug)]
struct Dense<S>(Vec<S>);

impl<S> Dense<S>
where
    S: StateID,
{
    fn new() -> Self {
        Dense(vec![fail_id(); 256])
    }

    #[inline]
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<S> Index<u8> for Dense<S> {
    type Output = S;

    #[inline]
    fn index(&self, i: u8) -> &S {
        // SAFETY: This is safe because all dense transitions have
        // exactly 256 elements, so all u8 values are valid indices.
        &self.0[i as usize]
    }
}

impl<S> IndexMut<u8> for Dense<S> {
    #[inline]
    fn index_mut(&mut self, i: u8) -> &mut S {
        // SAFETY: This is safe because all dense transitions have
        // exactly 256 elements, so all u8 values are valid indices.
        &mut self.0[i as usize]
    }
}

/// A representation of transitions in an NFA.
///
/// Transitions have either a sparse representation, which is slower for
/// lookups but uses less memory, or a dense representation, which is faster
/// for lookups but uses more memory. In the sparse representation, the absence
/// of a state implies a transition to `fail_id()`. Transitions to `dead_id()`
/// are still explicitly represented.
///
/// For the NFA, by default, we use a dense representation for transitions for
/// states close to the start state because it's likely these are the states
/// that will be most frequently visited.
#[derive(Clone, Debug)]
enum Transitions<S> {
    Sparse(Vec<(u8, S)>),
    Dense(Dense<S>),
}

impl<S: StateID> Transitions<S> {
    fn heap_bytes(&self) -> usize {
        match *self {
            Transitions::Sparse(ref sparse) => {
                sparse.len() * size_of::<(u8, S)>()
            }
            Transitions::Dense(ref dense) => dense.len() * size_of::<S>(),
        }
    }

    fn next_state(&self, input: u8) -> S {
        match *self {
            Transitions::Sparse(ref sparse) => {
                for &(b, id) in sparse {
                    if b == input {
                        return id;
                    }
                }
                fail_id()
            }
            Transitions::Dense(ref dense) => dense[input],
        }
    }

    fn set_next_state(&mut self, input: u8, next: S) {
        match *self {
            Transitions::Sparse(ref mut sparse) => {
                match sparse.binary_search_by_key(&input, |&(b, _)| b) {
                    Ok(i) => sparse[i] = (input, next),
                    Err(i) => sparse.insert(i, (input, next)),
                }
            }
            Transitions::Dense(ref mut dense) => {
                dense[input] = next;
            }
        }
    }

    /// Iterate over transitions in this state while skipping over transitions
    /// to `fail_id()`.
    fn iter<F: FnMut(u8, S)>(&self, mut f: F) {
        match *self {
            Transitions::Sparse(ref sparse) => {
                for &(b, id) in sparse {
                    f(b, id);
                }
            }
            Transitions::Dense(ref dense) => {
                for b in AllBytesIter::new() {
                    let id = dense[b];
                    if id != fail_id() {
                        f(b, id);
                    }
                }
            }
        }
    }

    /// Iterate over all transitions in this state according to the given
    /// equivalence classes, including transitions to `fail_id()`.
    fn iter_all<F: FnMut(u8, S)>(&self, classes: &ByteClasses, mut f: F) {
        if classes.is_singleton() {
            match *self {
                Transitions::Sparse(ref sparse) => {
                    sparse_iter(sparse, f);
                }
                Transitions::Dense(ref dense) => {
                    for b in AllBytesIter::new() {
                        f(b, dense[b]);
                    }
                }
            }
        } else {
            // In this case, we only want to yield a single byte for each
            // equivalence class.
            match *self {
                Transitions::Sparse(ref sparse) => {
                    let mut last_class = None;
                    sparse_iter(sparse, |b, next| {
                        let class = classes.get(b);
                        if last_class != Some(class) {
                            last_class = Some(class);
                            f(b, next);
                        }
                    })
                }
                Transitions::Dense(ref dense) => {
                    for b in classes.representatives() {
                        f(b, dense[b]);
                    }
                }
            }
        }
    }
}

/// Iterator over transitions in a state, skipping transitions to `fail_id()`.
///
/// This abstracts over the representation of NFA transitions, which may be
/// either in a sparse or dense representation.
///
/// This somewhat idiosyncratically borrows the NFA mutably, so that when one
/// is iterating over transitions, the caller can still mutate the NFA. This
/// is useful when creating failure transitions.
#[derive(Debug)]
struct IterTransitionsMut<'a, S: StateID> {
    nfa: &'a mut NFA<S>,
    state_id: S,
    cur: usize,
}

impl<'a, S: StateID> IterTransitionsMut<'a, S> {
    fn new(nfa: &'a mut NFA<S>, state_id: S) -> IterTransitionsMut<'a, S> {
        IterTransitionsMut { nfa, state_id, cur: 0 }
    }

    fn nfa(&mut self) -> &mut NFA<S> {
        self.nfa
    }
}

impl<'a, S: StateID> Iterator for IterTransitionsMut<'a, S> {
    type Item = (u8, S);

    fn next(&mut self) -> Option<(u8, S)> {
        match self.nfa.states[self.state_id.to_usize()].trans {
            Transitions::Sparse(ref sparse) => {
                if self.cur >= sparse.len() {
                    return None;
                }
                let i = self.cur;
                self.cur += 1;
                Some(sparse[i])
            }
            Transitions::Dense(ref dense) => {
                while self.cur < dense.len() {
                    // There are always exactly 255 transitions in dense repr.
                    debug_assert!(self.cur < 256);

                    let b = self.cur as u8;
                    let id = dense[b];
                    self.cur += 1;
                    if id != fail_id() {
                        return Some((b, id));
                    }
                }
                None
            }
        }
    }
}

/// A simple builder for configuring the NFA construction of Aho-Corasick.
#[derive(Clone, Debug)]
pub struct Builder {
    dense_depth: usize,
    match_kind: MatchKind,
    prefilter: bool,
    anchored: bool,
    ascii_case_insensitive: bool,
}

impl Default for Builder {
    fn default() -> Builder {
      crate::nfa::Builder { dense_depth: 0 as usize, match_kind: crate::ahocorasick::MatchKind::Standard, prefilter: true, anchored: true, ascii_case_insensitive: true }
    }






}

impl Builder {
    pub fn new() -> Builder {
      crate::nfa::Builder { dense_depth: 0 as usize, match_kind: crate::ahocorasick::MatchKind::Standard, prefilter: true, anchored: true, ascii_case_insensitive: true }
    }

    pub fn build<I, P, S: StateID>(&self, patterns: I) -> Result<NFA<S>>
    where
        I: IntoIterator<Item = P>,
        P: AsRef<[u8]>,
    {
        Compiler::new(self)?.compile(patterns)
    }

    pub fn match_kind(&mut self, kind: MatchKind) -> &mut Builder {
      self
    }


    pub fn dense_depth(&mut self, depth: usize) -> &mut Builder {
      self
    }


    pub fn prefilter(&mut self, yes: bool) -> &mut Builder {
      self
    }


    pub fn anchored(&mut self, yes: bool) -> &mut Builder {
      self
    }


    pub fn ascii_case_insensitive(&mut self, yes: bool) -> &mut Builder {
      self
    }

}

/// A compiler uses a builder configuration and builds up the NFA formulation
/// of an Aho-Corasick automaton. This roughly corresponds to the standard
/// formulation described in textbooks.
#[derive(Debug)]
struct Compiler<'a, S: StateID> {
    builder: &'a Builder,
    prefilter: prefilter::Builder,
    nfa: NFA<S>,
    byte_classes: ByteClassBuilder,
}

impl<'a, S: StateID> Compiler<'a, S> {
    fn new(builder: &'a Builder) -> Result<Compiler<'a, S>> {
        Ok(Compiler {
            builder,
            prefilter: prefilter::Builder::new(builder.match_kind)
                .ascii_case_insensitive(builder.ascii_case_insensitive),
            nfa: NFA {
                match_kind: builder.match_kind,
                start_id: usize_to_state_id(2)?,
                max_pattern_len: 0,
                pattern_count: 0,
                heap_bytes: 0,
                prefilter: None,
                anchored: builder.anchored,
                byte_classes: ByteClasses::singletons(),
                states: vec![],
            },
            byte_classes: ByteClassBuilder::new(),
        })
    }

    fn compile<I, P>(mut self, patterns: I) -> Result<NFA<S>>
    where
        I: IntoIterator<Item = P>,
        P: AsRef<[u8]>,
    {
        self.add_state(0)?; // the fail state, which is never entered
        self.add_state(0)?; // the dead state, only used for leftmost
        self.add_state(0)?; // the start state
        self.build_trie(patterns)?;
        self.add_start_state_loop();
        self.add_dead_state_loop();
        if !self.builder.anchored {
            self.fill_failure_transitions();
        }
        self.close_start_state_loop();
        self.nfa.byte_classes = self.byte_classes.build();
        if !self.builder.anchored {
            self.nfa.prefilter = self.prefilter.build();
        }
        self.calculate_size();
        Ok(self.nfa)
    }

    /// This sets up the initial prefix trie that makes up the Aho-Corasick
    /// automaton. Effectively, it creates the basic structure of the
    /// automaton, where every pattern given has a path from the start state to
    /// the end of the pattern.
    fn build_trie<I, P>(&mut self, patterns: I) -> Result<()>
    where
        I: IntoIterator<Item = P>,
        P: AsRef<[u8]>,
    {
        'PATTERNS: for (pati, pat) in patterns.into_iter().enumerate() {
            let pat = pat.as_ref();
            self.nfa.max_pattern_len =
                cmp::max(self.nfa.max_pattern_len, pat.len());
            self.nfa.pattern_count += 1;

            let mut prev = self.nfa.start_id;
            let mut saw_match = false;
            for (depth, &b) in pat.iter().enumerate() {
                // When leftmost-first match semantics are requested, we
                // specifically stop adding patterns when a previously added
                // pattern is a prefix of it. We avoid adding it because
                // leftmost-first semantics imply that the pattern can never
                // match. This is not just an optimization to save space! It
                // is necessary for correctness. In fact, this is the only
                // difference in the automaton between the implementations for
                // leftmost-first and leftmost-longest.
                saw_match = saw_match || self.nfa.state(prev).is_match();
                if self.builder.match_kind.is_leftmost_first() && saw_match {
                    // Skip to the next pattern immediately. This avoids
                    // incorrectly adding a match after this loop terminates.
                    continue 'PATTERNS;
                }

                // Add this byte to our equivalence classes. We don't use these
                // for NFA construction. These are instead used only if we're
                // building a DFA. They would technically be useful for the
                // NFA, but it would require a second pass over the patterns.
                self.byte_classes.set_range(b, b);
                if self.builder.ascii_case_insensitive {
                    let b = opposite_ascii_case(b);
                    self.byte_classes.set_range(b, b);
                }

                // If the transition from prev using the current byte already
                // exists, then just move through it. Otherwise, add a new
                // state. We track the depth here so that we can determine
                // how to represent transitions. States near the start state
                // use a dense representation that uses more memory but is
                // faster. Other states use a sparse representation that uses
                // less memory but is slower.
                let next = self.nfa.state(prev).next_state(b);
                if next != fail_id() {
                    prev = next;
                } else {
                    let next = self.add_state(depth + 1)?;
                    self.nfa.state_mut(prev).set_next_state(b, next);
                    if self.builder.ascii_case_insensitive {
                        let b = opposite_ascii_case(b);
                        self.nfa.state_mut(prev).set_next_state(b, next);
                    }
                    prev = next;
                }
            }
            // Once the pattern has been added, log the match in the final
            // state that it reached.
            self.nfa.state_mut(prev).add_match(pati, pat.len());
            // ... and hand it to the prefilter builder, if applicable.
            if self.builder.prefilter {
                self.prefilter.add(pat);
            }
        }
        Ok(())
    }

    /// This routine creates failure transitions according to the standard
    /// textbook formulation of the Aho-Corasick algorithm, with a couple small
    /// tweaks to support "leftmost" semantics.
    ///
    /// Building failure transitions is the most interesting part of building
    /// the Aho-Corasick automaton, because they are what allow searches to
    /// be performed in linear time. Specifically, a failure transition is
    /// a single transition associated with each state that points back to
    /// the longest proper suffix of the pattern being searched. The failure
    /// transition is followed whenever there exists no transition on the
    /// current state for the current input byte. If there is no other proper
    /// suffix, then the failure transition points back to the starting state.
    ///
    /// For example, let's say we built an Aho-Corasick automaton with the
    /// following patterns: 'abcd' and 'cef'. The trie looks like this:
    ///
    /// ```ignore
    ///          a - S1 - b - S2 - c - S3 - d - S4*
    ///         /
    ///     S0 - c - S5 - e - S6 - f - S7*
    /// ```
    ///
    /// At this point, it should be fairly straight-forward to see how this
    /// trie can be used in a simplistic way. At any given position in the
    /// text we're searching (called the "subject" string), all we need to do
    /// is follow the transitions in the trie by consuming one transition for
    /// each byte in the subject string. If we reach a match state, then we can
    /// report that location as a match.
    ///
    /// The trick comes when searching a subject string like 'abcef'. We'll
    /// initially follow the transition from S0 to S1 and wind up in S3 after
    /// observng the 'c' byte. At this point, the next byte is 'e' but state
    /// S3 has no transition for 'e', so the search fails. We then would need
    /// to restart the search at the next position in 'abcef', which
    /// corresponds to 'b'. The match would fail, but the next search starting
    /// at 'c' would finally succeed. The problem with this approach is that
    /// we wind up searching the subject string potentially many times. In
    /// effect, this makes the algorithm have worst case `O(n * m)` complexity,
    /// where `n ~ len(subject)` and `m ~ len(all patterns)`. We would instead
    /// like to achieve a `O(n + m)` worst case complexity.
    ///
    /// This is where failure transitions come in. Instead of dying at S3 in
    /// the first search, the automaton can instruct the search to move to
    /// another part of the automaton that corresponds to a suffix of what
    /// we've seen so far. Recall that we've seen 'abc' in the subject string,
    /// and the automaton does indeed have a non-empty suffix, 'c', that could
    /// potentially lead to another match. Thus, the actual Aho-Corasick
    /// automaton for our patterns in this case looks like this:
    ///
    /// ```ignore
    ///          a - S1 - b - S2 - c - S3 - d - S4*
    ///         /                      /
    ///        /       ----------------
    ///       /       /
    ///     S0 - c - S5 - e - S6 - f - S7*
    /// ```
    ///
    /// That is, we have a failure transition from S3 to S5, which is followed
    /// exactly in cases when we are in state S3 but see any byte other than
    /// 'd' (that is, we've "failed" to find a match in this portion of our
    /// trie). We know we can transition back to S5 because we've already seen
    /// a 'c' byte, so we don't need to re-scan it. We can then pick back up
    /// with the search starting at S5 and complete our match.
    ///
    /// Adding failure transitions to a trie is fairly simple, but subtle. The
    /// key issue is that you might have multiple failure transition that you
    /// need to follow. For example, look at the trie for the patterns
    /// 'abcd', 'b', 'bcd' and 'cd':
    ///
    /// ```ignore
    ///          - a - S1 - b - S2* - c - S3 - d - S4*
    ///         /               /         /
    ///        /         -------   -------
    ///       /         /         /
    ///     S0 --- b - S5* - c - S6 - d - S7*
    ///       \                  /
    ///        \         --------
    ///         \       /
    ///          - c - S8 - d - S9*
    /// ```
    ///
    /// The failure transitions for this trie are defined from S2 to S5,
    /// S3 to S6 and S6 to S8. Moreover, state S2 needs to track that it
    /// corresponds to a match, since its failure transition to S5 is itself
    /// a match state.
    ///
    /// Perhaps simplest way to think about adding these failure transitions
    /// is recursively. That is, if you know the failure transitions for every
    /// possible previous state that could be visited (e.g., when computing the
    /// failure transition for S3, you already know the failure transitions
    /// for S0, S1 and S2), then you can simply follow the failure transition
    /// of the previous state and check whether the incoming transition is
    /// defined after following the failure transition.
    ///
    /// For example, when determining the failure state for S3, by our
    /// assumptions, we already know that there is a failure transition from
    /// S2 (the previous state) to S5. So we follow that transition and check
    /// whether the transition connecting S2 to S3 is defined. Indeed, it is,
    /// as there is a transition from S5 to S6 for the byte 'c'. If no such
    /// transition existed, we could keep following the failure transitions
    /// until we reach the start state, which is the failure transition for
    /// every state that has no corresponding proper suffix.
    ///
    /// We don't actually use recursion to implement this, but instead, use a
    /// breadth first search of the automaton. Our base case is the start
    /// state, whose failure transition is just a transition to itself.
    ///
    /// When building a leftmost automaton, we proceed as above, but only
    /// include a subset of failure transitions. Namely, we omit any failure
    /// transitions that appear after a match state in the trie. This is
    /// because failure transitions always point back to a proper suffix of
    /// what has been seen so far. Thus, following a failure transition after
    /// a match implies looking for a match that starts after the one that has
    /// already been seen, which is of course therefore not the leftmost match.
    ///
    /// N.B. I came up with this algorithm on my own, and after scouring all of
    /// the other AC implementations I know of (Perl, Snort, many on GitHub).
    /// I couldn't find any that implement leftmost semantics like this.
    /// Perl of course needs leftmost-first semantics, but they implement it
    /// with a seeming hack at *search* time instead of encoding it into the
    /// automaton. There are also a couple Java libraries that support leftmost
    /// longest semantics, but they do it by building a queue of matches at
    /// search time, which is even worse than what Perl is doing. ---AG
    fn fill_failure_transitions(&mut self) {
        let kind = self.match_kind();
        // Initialize the queue for breadth first search with all transitions
        // out of the start state. We handle the start state specially because
        // we only want to follow non-self transitions. If we followed self
        // transitions, then this would never terminate.
        let mut queue = VecDeque::new();
        let mut seen = self.queued_set();
        let mut it = self.nfa.iter_transitions_mut(self.nfa.start_id);
        while let Some((_, next)) = it.next() {
            // Skip anything we've seen before and any self-transitions on the
            // start state.
            if next == it.nfa().start_id || seen.contains(next) {
                continue;
            }
            queue.push_back(next);
            seen.insert(next);
            // Under leftmost semantics, if a state immediately following
            // the start state is a match state, then we never want to
            // follow its failure transition since the failure transition
            // necessarily leads back to the start state, which we never
            // want to do for leftmost matching after a match has been
            // found.
            //
            // We apply the same logic to non-start states below as well.
            if kind.is_leftmost() && it.nfa().state(next).is_match() {
                it.nfa().state_mut(next).fail = dead_id();
            }
        }
        while let Some(id) = queue.pop_front() {
            let mut it = self.nfa.iter_transitions_mut(id);
            while let Some((b, next)) = it.next() {
                if seen.contains(next) {
                    // The only way to visit a duplicate state in a transition
                    // list is when ASCII case insensitivity is enabled. In
                    // this case, we want to skip it since it's redundant work.
                    // But it would also end up duplicating matches, which
                    // results in reporting duplicate matches in some cases.
                    // See the 'acasei010' regression test.
                    continue;
                }
                queue.push_back(next);
                seen.insert(next);

                // As above for start states, under leftmost semantics, once
                // we see a match all subsequent states should have no failure
                // transitions because failure transitions always imply looking
                // for a match that is a suffix of what has been seen so far
                // (where "seen so far" corresponds to the string formed by
                // following the transitions from the start state to the
                // current state). Under leftmost semantics, we specifically do
                // not want to allow this to happen because we always want to
                // report the match found at the leftmost position.
                //
                // The difference between leftmost-first and leftmost-longest
                // occurs previously while we build the trie. For
                // leftmost-first, we simply omit any entries that would
                // otherwise require passing through a match state.
                //
                // Note that for correctness, the failure transition has to be
                // set to the dead state for ALL states following a match, not
                // just the match state itself. However, by setting the failure
                // transition to the dead state on all match states, the dead
                // state will automatically propagate to all subsequent states
                // via the failure state computation below.
                if kind.is_leftmost() && it.nfa().state(next).is_match() {
                    it.nfa().state_mut(next).fail = dead_id();
                    continue;
                }
                let mut fail = it.nfa().state(id).fail;
                while it.nfa().state(fail).next_state(b) == fail_id() {
                    fail = it.nfa().state(fail).fail;
                }
                fail = it.nfa().state(fail).next_state(b);
                it.nfa().state_mut(next).fail = fail;
                it.nfa().copy_matches(fail, next);
            }
            // If the start state is a match state, then this automaton can
            // match the empty string. This implies all states are match states
            // since every position matches the empty string, so copy the
            // matches from the start state to every state. Strictly speaking,
            // this is only necessary for overlapping matches since each
            // non-empty non-start match state needs to report empty matches
            // in addition to its own. For the non-overlapping case, such
            // states only report the first match, which is never empty since
            // it isn't a start state.
            if !kind.is_leftmost() {
                it.nfa().copy_empty_matches(id);
            }
        }
    }

    /// Returns a set that tracked queued states.
    ///
    /// This is only necessary when ASCII case insensitivity is enabled, since
    /// it is the only way to visit the same state twice. Otherwise, this
    /// returns an inert set that nevers adds anything and always reports
    /// `false` for every member test.
    fn queued_set(&self) -> QueuedSet<S> {
        if self.builder.ascii_case_insensitive {
            QueuedSet::active()
        } else {
            QueuedSet::inert()
        }
    }

    /// Set the failure transitions on the start state to loop back to the
    /// start state. This effectively permits the Aho-Corasick automaton to
    /// match at any position. This is also required for finding the next
    /// state to terminate, namely, finding the next state should never return
    /// a fail_id.
    ///
    /// This must be done after building the initial trie, since trie
    /// construction depends on transitions to `fail_id` to determine whether a
    /// state already exists or not.
    fn add_start_state_loop(&mut self) {
        let start_id = self.nfa.start_id;
        let start = self.nfa.start_mut();
        for b in AllBytesIter::new() {
            if start.next_state(b) == fail_id() {
                start.set_next_state(b, start_id);
            }
        }
    }

    /// Remove the start state loop by rewriting any transitions on the start
    /// state back to the start state with transitions to the dead state.
    ///
    /// The loop is only closed when two conditions are met: the start state
    /// is a match state and the match kind is leftmost-first or
    /// leftmost-longest. (Alternatively, if this is an anchored automaton,
    /// then the start state is always closed, regardless of aforementioned
    /// conditions.)
    ///
    /// The reason for this is that under leftmost semantics, a start state
    /// that is also a match implies that we should never restart the search
    /// process. We allow normal transitions out of the start state, but if
    /// none exist, we transition to the dead state, which signals that
    /// searching should stop.
    fn close_start_state_loop(&mut self) {
        if self.builder.anchored
            || (self.match_kind().is_leftmost() && self.nfa.start().is_match())
        {
            let start_id = self.nfa.start_id;
            let start = self.nfa.start_mut();
            for b in AllBytesIter::new() {
                if start.next_state(b) == start_id {
                    start.set_next_state(b, dead_id());
                }
            }
        }
    }

    /// Sets all transitions on the dead state to point back to the dead state.
    /// Normally, missing transitions map back to the failure state, but the
    /// point of the dead state is to act as a sink that can never be escaped.
    fn add_dead_state_loop(&mut self) {
        let dead = self.nfa.state_mut(dead_id());
        for b in AllBytesIter::new() {
            dead.set_next_state(b, dead_id());
        }
    }

    /// Computes the total amount of heap used by this NFA in bytes.
    fn calculate_size(&mut self) {
        let mut size = 0;
        for state in &self.nfa.states {
            size += size_of::<State<S>>() + state.heap_bytes();
        }
        self.nfa.heap_bytes = size;
    }

    /// Add a new state to the underlying NFA with the given depth. The depth
    /// is used to determine how to represent the transitions.
    ///
    /// If adding the new state would overflow the chosen state ID
    /// representation, then this returns an error.
    fn add_state(&mut self, depth: usize) -> Result<S> {
        if depth < self.builder.dense_depth {
            self.nfa.add_dense_state(depth)
        } else {
            self.nfa.add_sparse_state(depth)
        }
    }

    /// Returns the match kind configured on the underlying builder.
    fn match_kind(&self) -> MatchKind {
        self.builder.match_kind
    }
}

/// A set of state identifiers used to avoid revisiting the same state multiple
/// times when filling in failure transitions.
///
/// This set has an "inert" and an "active" mode. When inert, the set never
/// stores anything and always returns `false` for every member test. This is
/// useful to avoid the performance and memory overhead of maintaining this
/// set when it is not needed.
#[derive(Debug)]
struct QueuedSet<S> {
    set: Option<BTreeSet<S>>,
}

impl<S: StateID> QueuedSet<S> {
    /// Return an inert set that returns `false` for every state ID membership
    /// test.
    fn inert() -> QueuedSet<S> {
        QueuedSet { set: None }
    }

    /// Return an active set that tracks state ID membership.
    fn active() -> QueuedSet<S> {
        QueuedSet { set: Some(BTreeSet::new()) }
    }

    /// Inserts the given state ID into this set. (If the set is inert, then
    /// this is a no-op.)
    fn insert(&mut self, state_id: S) {
        if let Some(ref mut set) = self.set {
            set.insert(state_id);
        }
    }

    /// Returns true if and only if the given state ID is in this set. If the
    /// set is inert, this always returns false.
    fn contains(&self, state_id: S) -> bool {
        match self.set {
            None => false,
            Some(ref set) => set.contains(&state_id),
        }
    }
}

/// An iterator over every byte value.
///
/// We use this instead of (0..256).map(|b| b as u8) because this optimizes
/// better in debug builds.
///
/// We also use this instead of 0..=255 because we're targeting Rust 1.24 and
/// inclusive range syntax was stabilized in Rust 1.26. We can get rid of this
/// once our MSRV is Rust 1.26 or newer.
#[derive(Debug)]
struct AllBytesIter(u16);

impl AllBytesIter {
    fn new() -> AllBytesIter {
      crate::nfa::AllBytesIter(0 as u16)
    }
}

impl Iterator for AllBytesIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 >= 256 {
            None
        } else {
            let b = self.0 as u8;
            self.0 += 1;
            Some(b)
        }
    }
}

impl<S: StateID> fmt::Debug for NFA<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "NFA(")?;
        writeln!(f, "match_kind: {:?}", self.match_kind)?;
        writeln!(f, "prefilter: {:?}", self.prefilter)?;
        writeln!(f, "{}", "-".repeat(79))?;
        for (id, s) in self.states.iter().enumerate() {
            let mut trans = vec![];
            s.trans.iter(|byte, next| {
                // The start state has a bunch of uninteresting transitions
                // back into itself. It's questionable to hide them since they
                // are critical to understanding the automaton, but they are
                // very noisy without better formatting for contiugous ranges
                // to the same state.
                if id == self.start_id.to_usize() && next == self.start_id {
                    return;
                }
                // Similarly, the dead state has a bunch of uninteresting
                // transitions too.
                if id == dead_id() {
                    return;
                }
                trans.push(format!("{} => {}", escape(byte), next.to_usize()));
            });
            writeln!(f, "{:04}: {}", id, trans.join(", "))?;

            let matches: Vec<String> = s
                .matches
                .iter()
                .map(|&(pattern_id, _)| pattern_id.to_string())
                .collect();
            writeln!(f, "  matches: {}", matches.join(", "))?;
            writeln!(f, "     fail: {}", s.fail.to_usize())?;
            writeln!(f, "    depth: {}", s.depth)?;
        }
        writeln!(f, "{}", "-".repeat(79))?;
        writeln!(f, ")")?;
        Ok(())
    }
}

/// Iterate over all possible byte transitions given a sparse set.
fn sparse_iter<S: StateID, F: FnMut(u8, S)>(trans: &[(u8, S)], mut f: F) {
    let mut byte = 0u16;
    for &(b, id) in trans {
        while byte < (b as u16) {
            f(byte as u8, fail_id());
            byte += 1;
        }
        f(b, id);
        byte += 1;
    }
    for b in byte..256 {
        f(b as u8, fail_id());
    }
}

/// Safely return two mutable borrows to two different locations in the given
/// slice.
///
/// This panics if i == j.
fn get_two_mut<T>(xs: &mut [T], i: usize, j: usize) -> (&mut T, &mut T) {
    assert!(i != j, "{} must not be equal to {}", i, j);
    if i < j {
        let (before, after) = xs.split_at_mut(j);
        (&mut before[i], &mut after[0])
    } else {
        let (before, after) = xs.split_at_mut(i);
        (&mut after[0], &mut before[j])
    }
}

/// Return the given byte as its escaped string form.
fn escape(b: u8) -> String {
    use std::ascii;

    String::from_utf8(ascii::escape_default(b).collect::<Vec<_>>()).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scratch() {
        let nfa: NFA<usize> = Builder::new()
            .dense_depth(0)
            // .match_kind(MatchKind::LeftmostShortest)
            // .match_kind(MatchKind::LeftmostLongest)
            .match_kind(MatchKind::LeftmostFirst)
            // .build(&["abcd", "ce", "b"])
            // .build(&["ab", "bc"])
            // .build(&["b", "bcd", "ce"])
            // .build(&["abc", "bx"])
            // .build(&["abc", "bd", "ab"])
            // .build(&["abcdefghi", "hz", "abcdefgh"])
            // .build(&["abcd", "bce", "b"])
            .build(&["abcdefg", "bcde", "bcdef"])
            .unwrap();
        println!("{:?}", nfa);
    }
}
