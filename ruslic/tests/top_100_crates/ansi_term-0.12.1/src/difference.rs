use super::Style;


/// When printing out one coloured string followed by another, use one of
/// these rules to figure out which *extra* control codes need to be sent.
#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Difference {

    /// Print out the control codes specified by this style to end up looking
    /// like the second string's styles.
    ExtraStyles(Style),

    /// Converting between these two is impossible, so just send a reset
    /// command and then the second string's styles.
    Reset,

    /// The before style is exactly the same as the after style, so no further
    /// control codes need to be printed.
    NoDifference,
}


impl Difference {

    /// Compute the 'style difference' required to turn an existing style into
    /// the given, second style.
    ///
    /// For example, to turn green text into green bold text, it's redundant
    /// to write a reset command then a second green+bold command, instead of
    /// just writing one bold command. This method should see that both styles
    /// use the foreground colour green, and reduce it to a single command.
    ///
    /// This method returns an enum value because it's not actually always
    /// possible to turn one style into another: for example, text could be
    /// made bold and underlined, but you can't remove the bold property
    /// without also removing the underline property. So when this has to
    /// happen, this function returns None, meaning that the entire set of
    /// styles should be reset and begun again.
    pub fn between(first: &Style, next: &Style) -> Difference {
      crate::difference::Difference::Reset
    }
































































































}


#[cfg(test)]
mod test {
    use super::*;
    use super::Difference::*;
    use style::Colour::*;
    use style::Style;

    fn style() -> Style {
        Style::new()
    }

    macro_rules! test {
        ($name: ident: $first: expr; $next: expr => $result: expr) => {
            #[test]
            fn $name() {
                assert_eq!($result, Difference::between(&$first, &$next));
            }
        };
    }

    test!(nothing:    Green.normal(); Green.normal()  => NoDifference);
    test!(uppercase:  Green.normal(); Green.bold()    => ExtraStyles(style().bold()));
    test!(lowercase:  Green.bold();   Green.normal()  => Reset);
    test!(nothing2:   Green.bold();   Green.bold()    => NoDifference);

    test!(colour_change: Red.normal(); Blue.normal() => ExtraStyles(Blue.normal()));

    test!(addition_of_blink:          style(); style().blink()          => ExtraStyles(style().blink()));
    test!(addition_of_dimmed:         style(); style().dimmed()         => ExtraStyles(style().dimmed()));
    test!(addition_of_hidden:         style(); style().hidden()         => ExtraStyles(style().hidden()));
    test!(addition_of_reverse:        style(); style().reverse()        => ExtraStyles(style().reverse()));
    test!(addition_of_strikethrough:  style(); style().strikethrough()  => ExtraStyles(style().strikethrough()));

    test!(removal_of_strikethrough:   style().strikethrough(); style()  => Reset);
    test!(removal_of_reverse:         style().reverse();       style()  => Reset);
    test!(removal_of_hidden:          style().hidden();        style()  => Reset);
    test!(removal_of_dimmed:          style().dimmed();        style()  => Reset);
    test!(removal_of_blink:           style().blink();         style()  => Reset);
}
