// DO NOT EDIT THIS FILE. IT WAS AUTOMATICALLY GENERATED BY:
//
//   ucd-generate grapheme-cluster-break ucd-15.0.0 --chars
//
// Unicode version: 15.0.0.
//
// ucd-generate 0.2.14 is available on crates.io.

pub const BY_NAME: &'static [(&'static str, &'static [(char, char)])] = &[
    ("CR", CR),
    ("Control", CONTROL),
    ("Extend", EXTEND),
    ("L", L),
    ("LF", LF),
    ("LV", LV),
    ("LVT", LVT),
    ("Prepend", PREPEND),
    ("Regional_Indicator", REGIONAL_INDICATOR),
    ("SpacingMark", SPACINGMARK),
    ("T", T),
    ("V", V),
    ("ZWJ", ZWJ),
];

pub const CR: &'static [(char, char)] = &[('\r', '\r')];

pub const CONTROL: &'static [(char, char)] = &[
    ('\0', '\t'),
    ('\u{b}', '\u{c}'),
    ('\u{e}', '\u{1f}'),
    ('\u{7f}', '\u{9f}'),
    ('\u{ad}', '\u{ad}'),
    ('\u{61c}', '\u{61c}'),
    ('\u{180e}', '\u{180e}'),
    ('\u{200b}', '\u{200b}'),
    ('\u{200e}', '\u{200f}'),
    ('\u{2028}', '\u{202e}'),
    ('\u{2060}', '\u{206f}'),
    ('\u{feff}', '\u{feff}'),
    ('\u{fff0}', '\u{fffb}'),
    ('\u{13430}', '\u{1343f}'),
    ('\u{1bca0}', '\u{1bca3}'),
    ('\u{1d173}', '\u{1d17a}'),
    ('\u{e0000}', '\u{e001f}'),
    ('\u{e0080}', '\u{e00ff}'),
    ('\u{e01f0}', '\u{e0fff}'),
];

pub const EXTEND: &'static [(char, char)] = &[
    ('\u{300}', '\u{36f}'),
    ('\u{483}', '\u{489}'),
    ('\u{591}', '\u{5bd}'),
    ('\u{5bf}', '\u{5bf}'),
    ('\u{5c1}', '\u{5c2}'),
    ('\u{5c4}', '\u{5c5}'),
    ('\u{5c7}', '\u{5c7}'),
    ('\u{610}', '\u{61a}'),
    ('\u{64b}', '\u{65f}'),
    ('\u{670}', '\u{670}'),
    ('\u{6d6}', '\u{6dc}'),
    ('\u{6df}', '\u{6e4}'),
    ('\u{6e7}', '\u{6e8}'),
    ('\u{6ea}', '\u{6ed}'),
    ('\u{711}', '\u{711}'),
    ('\u{730}', '\u{74a}'),
    ('\u{7a6}', '\u{7b0}'),
    ('\u{7eb}', '\u{7f3}'),
    ('\u{7fd}', '\u{7fd}'),
    ('\u{816}', '\u{819}'),
    ('\u{81b}', '\u{823}'),
    ('\u{825}', '\u{827}'),
    ('\u{829}', '\u{82d}'),
    ('\u{859}', '\u{85b}'),
    ('\u{898}', '\u{89f}'),
    ('\u{8ca}', '\u{8e1}'),
    ('\u{8e3}', '\u{902}'),
    ('\u{93a}', '\u{93a}'),
    ('\u{93c}', '\u{93c}'),
    ('\u{941}', '\u{948}'),
    ('\u{94d}', '\u{94d}'),
    ('\u{951}', '\u{957}'),
    ('\u{962}', '\u{963}'),
    ('\u{981}', '\u{981}'),
    ('\u{9bc}', '\u{9bc}'),
    ('\u{9be}', '\u{9be}'),
    ('\u{9c1}', '\u{9c4}'),
    ('\u{9cd}', '\u{9cd}'),
    ('\u{9d7}', '\u{9d7}'),
    ('\u{9e2}', '\u{9e3}'),
    ('\u{9fe}', '\u{9fe}'),
    ('\u{a01}', '\u{a02}'),
    ('\u{a3c}', '\u{a3c}'),
    ('\u{a41}', '\u{a42}'),
    ('\u{a47}', '\u{a48}'),
    ('\u{a4b}', '\u{a4d}'),
    ('\u{a51}', '\u{a51}'),
    ('\u{a70}', '\u{a71}'),
    ('\u{a75}', '\u{a75}'),
    ('\u{a81}', '\u{a82}'),
    ('\u{abc}', '\u{abc}'),
    ('\u{ac1}', '\u{ac5}'),
    ('\u{ac7}', '\u{ac8}'),
    ('\u{acd}', '\u{acd}'),
    ('\u{ae2}', '\u{ae3}'),
    ('\u{afa}', '\u{aff}'),
    ('\u{b01}', '\u{b01}'),
    ('\u{b3c}', '\u{b3c}'),
    ('\u{b3e}', '\u{b3f}'),
    ('\u{b41}', '\u{b44}'),
    ('\u{b4d}', '\u{b4d}'),
    ('\u{b55}', '\u{b57}'),
    ('\u{b62}', '\u{b63}'),
    ('\u{b82}', '\u{b82}'),
    ('\u{bbe}', '\u{bbe}'),
    ('\u{bc0}', '\u{bc0}'),
    ('\u{bcd}', '\u{bcd}'),
    ('\u{bd7}', '\u{bd7}'),
    ('\u{c00}', '\u{c00}'),
    ('\u{c04}', '\u{c04}'),
    ('\u{c3c}', '\u{c3c}'),
    ('\u{c3e}', '\u{c40}'),
    ('\u{c46}', '\u{c48}'),
    ('\u{c4a}', '\u{c4d}'),
    ('\u{c55}', '\u{c56}'),
    ('\u{c62}', '\u{c63}'),
    ('\u{c81}', '\u{c81}'),
    ('\u{cbc}', '\u{cbc}'),
    ('\u{cbf}', '\u{cbf}'),
    ('\u{cc2}', '\u{cc2}'),
    ('\u{cc6}', '\u{cc6}'),
    ('\u{ccc}', '\u{ccd}'),
    ('\u{cd5}', '\u{cd6}'),
    ('\u{ce2}', '\u{ce3}'),
    ('\u{d00}', '\u{d01}'),
    ('\u{d3b}', '\u{d3c}'),
    ('\u{d3e}', '\u{d3e}'),
    ('\u{d41}', '\u{d44}'),
    ('\u{d4d}', '\u{d4d}'),
    ('\u{d57}', '\u{d57}'),
    ('\u{d62}', '\u{d63}'),
    ('\u{d81}', '\u{d81}'),
    ('\u{dca}', '\u{dca}'),
    ('\u{dcf}', '\u{dcf}'),
    ('\u{dd2}', '\u{dd4}'),
    ('\u{dd6}', '\u{dd6}'),
    ('\u{ddf}', '\u{ddf}'),
    ('\u{e31}', '\u{e31}'),
    ('\u{e34}', '\u{e3a}'),
    ('\u{e47}', '\u{e4e}'),
    ('\u{eb1}', '\u{eb1}'),
    ('\u{eb4}', '\u{ebc}'),
    ('\u{ec8}', '\u{ece}'),
    ('\u{f18}', '\u{f19}'),
    ('\u{f35}', '\u{f35}'),
    ('\u{f37}', '\u{f37}'),
    ('\u{f39}', '\u{f39}'),
    ('\u{f71}', '\u{f7e}'),
    ('\u{f80}', '\u{f84}'),
    ('\u{f86}', '\u{f87}'),
    ('\u{f8d}', '\u{f97}'),
    ('\u{f99}', '\u{fbc}'),
    ('\u{fc6}', '\u{fc6}'),
    ('\u{102d}', '\u{1030}'),
    ('\u{1032}', '\u{1037}'),
    ('\u{1039}', '\u{103a}'),
    ('\u{103d}', '\u{103e}'),
    ('\u{1058}', '\u{1059}'),
    ('\u{105e}', '\u{1060}'),
    ('\u{1071}', '\u{1074}'),
    ('\u{1082}', '\u{1082}'),
    ('\u{1085}', '\u{1086}'),
    ('\u{108d}', '\u{108d}'),
    ('\u{109d}', '\u{109d}'),
    ('\u{135d}', '\u{135f}'),
    ('\u{1712}', '\u{1714}'),
    ('\u{1732}', '\u{1733}'),
    ('\u{1752}', '\u{1753}'),
    ('\u{1772}', '\u{1773}'),
    ('\u{17b4}', '\u{17b5}'),
    ('\u{17b7}', '\u{17bd}'),
    ('\u{17c6}', '\u{17c6}'),
    ('\u{17c9}', '\u{17d3}'),
    ('\u{17dd}', '\u{17dd}'),
    ('\u{180b}', '\u{180d}'),
    ('\u{180f}', '\u{180f}'),
    ('\u{1885}', '\u{1886}'),
    ('\u{18a9}', '\u{18a9}'),
    ('\u{1920}', '\u{1922}'),
    ('\u{1927}', '\u{1928}'),
    ('\u{1932}', '\u{1932}'),
    ('\u{1939}', '\u{193b}'),
    ('\u{1a17}', '\u{1a18}'),
    ('\u{1a1b}', '\u{1a1b}'),
    ('\u{1a56}', '\u{1a56}'),
    ('\u{1a58}', '\u{1a5e}'),
    ('\u{1a60}', '\u{1a60}'),
    ('\u{1a62}', '\u{1a62}'),
    ('\u{1a65}', '\u{1a6c}'),
    ('\u{1a73}', '\u{1a7c}'),
    ('\u{1a7f}', '\u{1a7f}'),
    ('\u{1ab0}', '\u{1ace}'),
    ('\u{1b00}', '\u{1b03}'),
    ('\u{1b34}', '\u{1b3a}'),
    ('\u{1b3c}', '\u{1b3c}'),
    ('\u{1b42}', '\u{1b42}'),
    ('\u{1b6b}', '\u{1b73}'),
    ('\u{1b80}', '\u{1b81}'),
    ('\u{1ba2}', '\u{1ba5}'),
    ('\u{1ba8}', '\u{1ba9}'),
    ('\u{1bab}', '\u{1bad}'),
    ('\u{1be6}', '\u{1be6}'),
    ('\u{1be8}', '\u{1be9}'),
    ('\u{1bed}', '\u{1bed}'),
    ('\u{1bef}', '\u{1bf1}'),
    ('\u{1c2c}', '\u{1c33}'),
    ('\u{1c36}', '\u{1c37}'),
    ('\u{1cd0}', '\u{1cd2}'),
    ('\u{1cd4}', '\u{1ce0}'),
    ('\u{1ce2}', '\u{1ce8}'),
    ('\u{1ced}', '\u{1ced}'),
    ('\u{1cf4}', '\u{1cf4}'),
    ('\u{1cf8}', '\u{1cf9}'),
    ('\u{1dc0}', '\u{1dff}'),
    ('\u{200c}', '\u{200c}'),
    ('\u{20d0}', '\u{20f0}'),
    ('\u{2cef}', '\u{2cf1}'),
    ('\u{2d7f}', '\u{2d7f}'),
    ('\u{2de0}', '\u{2dff}'),
    ('\u{302a}', '\u{302f}'),
    ('\u{3099}', '\u{309a}'),
    ('\u{a66f}', '\u{a672}'),
    ('\u{a674}', '\u{a67d}'),
    ('\u{a69e}', '\u{a69f}'),
    ('\u{a6f0}', '\u{a6f1}'),
    ('\u{a802}', '\u{a802}'),
    ('\u{a806}', '\u{a806}'),
    ('\u{a80b}', '\u{a80b}'),
    ('\u{a825}', '\u{a826}'),
    ('\u{a82c}', '\u{a82c}'),
    ('\u{a8c4}', '\u{a8c5}'),
    ('\u{a8e0}', '\u{a8f1}'),
    ('\u{a8ff}', '\u{a8ff}'),
    ('\u{a926}', '\u{a92d}'),
    ('\u{a947}', '\u{a951}'),
    ('\u{a980}', '\u{a982}'),
    ('\u{a9b3}', '\u{a9b3}'),
    ('\u{a9b6}', '\u{a9b9}'),
    ('\u{a9bc}', '\u{a9bd}'),
    ('\u{a9e5}', '\u{a9e5}'),
    ('\u{aa29}', '\u{aa2e}'),
    ('\u{aa31}', '\u{aa32}'),
    ('\u{aa35}', '\u{aa36}'),
    ('\u{aa43}', '\u{aa43}'),
    ('\u{aa4c}', '\u{aa4c}'),
    ('\u{aa7c}', '\u{aa7c}'),
    ('\u{aab0}', '\u{aab0}'),
    ('\u{aab2}', '\u{aab4}'),
    ('\u{aab7}', '\u{aab8}'),
    ('\u{aabe}', '\u{aabf}'),
    ('\u{aac1}', '\u{aac1}'),
    ('\u{aaec}', '\u{aaed}'),
    ('\u{aaf6}', '\u{aaf6}'),
    ('\u{abe5}', '\u{abe5}'),
    ('\u{abe8}', '\u{abe8}'),
    ('\u{abed}', '\u{abed}'),
    ('\u{fb1e}', '\u{fb1e}'),
    ('\u{fe00}', '\u{fe0f}'),
    ('\u{fe20}', '\u{fe2f}'),
    ('\u{ff9e}', '\u{ff9f}'),
    ('\u{101fd}', '\u{101fd}'),
    ('\u{102e0}', '\u{102e0}'),
    ('\u{10376}', '\u{1037a}'),
    ('\u{10a01}', '\u{10a03}'),
    ('\u{10a05}', '\u{10a06}'),
    ('\u{10a0c}', '\u{10a0f}'),
    ('\u{10a38}', '\u{10a3a}'),
    ('\u{10a3f}', '\u{10a3f}'),
    ('\u{10ae5}', '\u{10ae6}'),
    ('\u{10d24}', '\u{10d27}'),
    ('\u{10eab}', '\u{10eac}'),
    ('\u{10efd}', '\u{10eff}'),
    ('\u{10f46}', '\u{10f50}'),
    ('\u{10f82}', '\u{10f85}'),
    ('\u{11001}', '\u{11001}'),
    ('\u{11038}', '\u{11046}'),
    ('\u{11070}', '\u{11070}'),
    ('\u{11073}', '\u{11074}'),
    ('\u{1107f}', '\u{11081}'),
    ('\u{110b3}', '\u{110b6}'),
    ('\u{110b9}', '\u{110ba}'),
    ('\u{110c2}', '\u{110c2}'),
    ('\u{11100}', '\u{11102}'),
    ('\u{11127}', '\u{1112b}'),
    ('\u{1112d}', '\u{11134}'),
    ('\u{11173}', '\u{11173}'),
    ('\u{11180}', '\u{11181}'),
    ('\u{111b6}', '\u{111be}'),
    ('\u{111c9}', '\u{111cc}'),
    ('\u{111cf}', '\u{111cf}'),
    ('\u{1122f}', '\u{11231}'),
    ('\u{11234}', '\u{11234}'),
    ('\u{11236}', '\u{11237}'),
    ('\u{1123e}', '\u{1123e}'),
    ('\u{11241}', '\u{11241}'),
    ('\u{112df}', '\u{112df}'),
    ('\u{112e3}', '\u{112ea}'),
    ('\u{11300}', '\u{11301}'),
    ('\u{1133b}', '\u{1133c}'),
    ('\u{1133e}', '\u{1133e}'),
    ('\u{11340}', '\u{11340}'),
    ('\u{11357}', '\u{11357}'),
    ('\u{11366}', '\u{1136c}'),
    ('\u{11370}', '\u{11374}'),
    ('\u{11438}', '\u{1143f}'),
    ('\u{11442}', '\u{11444}'),
    ('\u{11446}', '\u{11446}'),
    ('\u{1145e}', '\u{1145e}'),
    ('\u{114b0}', '\u{114b0}'),
    ('\u{114b3}', '\u{114b8}'),
    ('\u{114ba}', '\u{114ba}'),
    ('\u{114bd}', '\u{114bd}'),
    ('\u{114bf}', '\u{114c0}'),
    ('\u{114c2}', '\u{114c3}'),
    ('\u{115af}', '\u{115af}'),
    ('\u{115b2}', '\u{115b5}'),
    ('\u{115bc}', '\u{115bd}'),
    ('\u{115bf}', '\u{115c0}'),
    ('\u{115dc}', '\u{115dd}'),
    ('\u{11633}', '\u{1163a}'),
    ('\u{1163d}', '\u{1163d}'),
    ('\u{1163f}', '\u{11640}'),
    ('\u{116ab}', '\u{116ab}'),
    ('\u{116ad}', '\u{116ad}'),
    ('\u{116b0}', '\u{116b5}'),
    ('\u{116b7}', '\u{116b7}'),
    ('\u{1171d}', '\u{1171f}'),
    ('\u{11722}', '\u{11725}'),
    ('\u{11727}', '\u{1172b}'),
    ('\u{1182f}', '\u{11837}'),
    ('\u{11839}', '\u{1183a}'),
    ('\u{11930}', '\u{11930}'),
    ('\u{1193b}', '\u{1193c}'),
    ('\u{1193e}', '\u{1193e}'),
    ('\u{11943}', '\u{11943}'),
    ('\u{119d4}', '\u{119d7}'),
    ('\u{119da}', '\u{119db}'),
    ('\u{119e0}', '\u{119e0}'),
    ('\u{11a01}', '\u{11a0a}'),
    ('\u{11a33}', '\u{11a38}'),
    ('\u{11a3b}', '\u{11a3e}'),
    ('\u{11a47}', '\u{11a47}'),
    ('\u{11a51}', '\u{11a56}'),
    ('\u{11a59}', '\u{11a5b}'),
    ('\u{11a8a}', '\u{11a96}'),
    ('\u{11a98}', '\u{11a99}'),
    ('\u{11c30}', '\u{11c36}'),
    ('\u{11c38}', '\u{11c3d}'),
    ('\u{11c3f}', '\u{11c3f}'),
    ('\u{11c92}', '\u{11ca7}'),
    ('\u{11caa}', '\u{11cb0}'),
    ('\u{11cb2}', '\u{11cb3}'),
    ('\u{11cb5}', '\u{11cb6}'),
    ('\u{11d31}', '\u{11d36}'),
    ('\u{11d3a}', '\u{11d3a}'),
    ('\u{11d3c}', '\u{11d3d}'),
    ('\u{11d3f}', '\u{11d45}'),
    ('\u{11d47}', '\u{11d47}'),
    ('\u{11d90}', '\u{11d91}'),
    ('\u{11d95}', '\u{11d95}'),
    ('\u{11d97}', '\u{11d97}'),
    ('\u{11ef3}', '\u{11ef4}'),
    ('\u{11f00}', '\u{11f01}'),
    ('\u{11f36}', '\u{11f3a}'),
    ('\u{11f40}', '\u{11f40}'),
    ('\u{11f42}', '\u{11f42}'),
    ('\u{13440}', '\u{13440}'),
    ('\u{13447}', '\u{13455}'),
    ('\u{16af0}', '\u{16af4}'),
    ('\u{16b30}', '\u{16b36}'),
    ('\u{16f4f}', '\u{16f4f}'),
    ('\u{16f8f}', '\u{16f92}'),
    ('\u{16fe4}', '\u{16fe4}'),
    ('\u{1bc9d}', '\u{1bc9e}'),
    ('\u{1cf00}', '\u{1cf2d}'),
    ('\u{1cf30}', '\u{1cf46}'),
    ('\u{1d165}', '\u{1d165}'),
    ('\u{1d167}', '\u{1d169}'),
    ('\u{1d16e}', '\u{1d172}'),
    ('\u{1d17b}', '\u{1d182}'),
    ('\u{1d185}', '\u{1d18b}'),
    ('\u{1d1aa}', '\u{1d1ad}'),
    ('\u{1d242}', '\u{1d244}'),
    ('\u{1da00}', '\u{1da36}'),
    ('\u{1da3b}', '\u{1da6c}'),
    ('\u{1da75}', '\u{1da75}'),
    ('\u{1da84}', '\u{1da84}'),
    ('\u{1da9b}', '\u{1da9f}'),
    ('\u{1daa1}', '\u{1daaf}'),
    ('\u{1e000}', '\u{1e006}'),
    ('\u{1e008}', '\u{1e018}'),
    ('\u{1e01b}', '\u{1e021}'),
    ('\u{1e023}', '\u{1e024}'),
    ('\u{1e026}', '\u{1e02a}'),
    ('\u{1e08f}', '\u{1e08f}'),
    ('\u{1e130}', '\u{1e136}'),
    ('\u{1e2ae}', '\u{1e2ae}'),
    ('\u{1e2ec}', '\u{1e2ef}'),
    ('\u{1e4ec}', '\u{1e4ef}'),
    ('\u{1e8d0}', '\u{1e8d6}'),
    ('\u{1e944}', '\u{1e94a}'),
    ('🏻', '🏿'),
    ('\u{e0020}', '\u{e007f}'),
    ('\u{e0100}', '\u{e01ef}'),
];

pub const L: &'static [(char, char)] = &[('ᄀ', 'ᅟ'), ('ꥠ', 'ꥼ')];

pub const LF: &'static [(char, char)] = &[('\n', '\n')];

pub const LV: &'static [(char, char)] = &[
    ('가', '가'),
    ('개', '개'),
    ('갸', '갸'),
    ('걔', '걔'),
    ('거', '거'),
    ('게', '게'),
    ('겨', '겨'),
    ('계', '계'),
    ('고', '고'),
    ('과', '과'),
    ('괘', '괘'),
    ('괴', '괴'),
    ('교', '교'),
    ('구', '구'),
    ('궈', '궈'),
    ('궤', '궤'),
    ('귀', '귀'),
    ('규', '규'),
    ('그', '그'),
    ('긔', '긔'),
    ('기', '기'),
    ('까', '까'),
    ('깨', '깨'),
    ('꺄', '꺄'),
    ('꺠', '꺠'),
    ('꺼', '꺼'),
    ('께', '께'),
    ('껴', '껴'),
    ('꼐', '꼐'),
    ('꼬', '꼬'),
    ('꽈', '꽈'),
    ('꽤', '꽤'),
    ('꾀', '꾀'),
    ('꾜', '꾜'),
    ('꾸', '꾸'),
    ('꿔', '꿔'),
    ('꿰', '꿰'),
    ('뀌', '뀌'),
    ('뀨', '뀨'),
    ('끄', '끄'),
    ('끠', '끠'),
    ('끼', '끼'),
    ('나', '나'),
    ('내', '내'),
    ('냐', '냐'),
    ('냬', '냬'),
    ('너', '너'),
    ('네', '네'),
    ('녀', '녀'),
    ('녜', '녜'),
    ('노', '노'),
    ('놔', '놔'),
    ('놰', '놰'),
    ('뇌', '뇌'),
    ('뇨', '뇨'),
    ('누', '누'),
    ('눠', '눠'),
    ('눼', '눼'),
    ('뉘', '뉘'),
    ('뉴', '뉴'),
    ('느', '느'),
    ('늬', '늬'),
    ('니', '니'),
    ('다', '다'),
    ('대', '대'),
    ('댜', '댜'),
    ('댸', '댸'),
    ('더', '더'),
    ('데', '데'),
    ('뎌', '뎌'),
    ('뎨', '뎨'),
    ('도', '도'),
    ('돠', '돠'),
    ('돼', '돼'),
    ('되', '되'),
    ('됴', '됴'),
    ('두', '두'),
    ('둬', '둬'),
    ('뒈', '뒈'),
    ('뒤', '뒤'),
    ('듀', '듀'),
    ('드', '드'),
    ('듸', '듸'),
    ('디', '디'),
    ('따', '따'),
    ('때', '때'),
    ('땨', '땨'),
    ('떄', '떄'),
    ('떠', '떠'),
    ('떼', '떼'),
    ('뗘', '뗘'),
    ('뗴', '뗴'),
    ('또', '또'),
    ('똬', '똬'),
    ('뙈', '뙈'),
    ('뙤', '뙤'),
    ('뚀', '뚀'),
    ('뚜', '뚜'),
    ('뚸', '뚸'),
    ('뛔', '뛔'),
    ('뛰', '뛰'),
    ('뜌', '뜌'),
    ('뜨', '뜨'),
    ('띄', '띄'),
    ('띠', '띠'),
    ('라', '라'),
    ('래', '래'),
    ('랴', '랴'),
    ('럐', '럐'),
    ('러', '러'),
    ('레', '레'),
    ('려', '려'),
    ('례', '례'),
    ('로', '로'),
    ('롸', '롸'),
    ('뢔', '뢔'),
    ('뢰', '뢰'),
    ('료', '료'),
    ('루', '루'),
    ('뤄', '뤄'),
    ('뤠', '뤠'),
    ('뤼', '뤼'),
    ('류', '류'),
    ('르', '르'),
    ('릐', '릐'),
    ('리', '리'),
    ('마', '마'),
    ('매', '매'),
    ('먀', '먀'),
    ('먜', '먜'),
    ('머', '머'),
    ('메', '메'),
    ('며', '며'),
    ('몌', '몌'),
    ('모', '모'),
    ('뫄', '뫄'),
    ('뫠', '뫠'),
    ('뫼', '뫼'),
    ('묘', '묘'),
    ('무', '무'),
    ('뭐', '뭐'),
    ('뭬', '뭬'),
    ('뮈', '뮈'),
    ('뮤', '뮤'),
    ('므', '므'),
    ('믜', '믜'),
    ('미', '미'),
    ('바', '바'),
    ('배', '배'),
    ('뱌', '뱌'),
    ('뱨', '뱨'),
    ('버', '버'),
    ('베', '베'),
    ('벼', '벼'),
    ('볘', '볘'),
    ('보', '보'),
    ('봐', '봐'),
    ('봬', '봬'),
    ('뵈', '뵈'),
    ('뵤', '뵤'),
    ('부', '부'),
    ('붜', '붜'),
    ('붸', '붸'),
    ('뷔', '뷔'),
    ('뷰', '뷰'),
    ('브', '브'),
    ('븨', '븨'),
    ('비', '비'),
    ('빠', '빠'),
    ('빼', '빼'),
    ('뺘', '뺘'),
    ('뺴', '뺴'),
    ('뻐', '뻐'),
    ('뻬', '뻬'),
    ('뼈', '뼈'),
    ('뼤', '뼤'),
    ('뽀', '뽀'),
    ('뽜', '뽜'),
    ('뽸', '뽸'),
    ('뾔', '뾔'),
    ('뾰', '뾰'),
    ('뿌', '뿌'),
    ('뿨', '뿨'),
    ('쀄', '쀄'),
    ('쀠', '쀠'),
    ('쀼', '쀼'),
    ('쁘', '쁘'),
    ('쁴', '쁴'),
    ('삐', '삐'),
    ('사', '사'),
    ('새', '새'),
    ('샤', '샤'),
    ('섀', '섀'),
    ('서', '서'),
    ('세', '세'),
    ('셔', '셔'),
    ('셰', '셰'),
    ('소', '소'),
    ('솨', '솨'),
    ('쇄', '쇄'),
    ('쇠', '쇠'),
    ('쇼', '쇼'),
    ('수', '수'),
    ('숴', '숴'),
    ('쉐', '쉐'),
    ('쉬', '쉬'),
    ('슈', '슈'),
    ('스', '스'),
    ('싀', '싀'),
    ('시', '시'),
    ('싸', '싸'),
    ('쌔', '쌔'),
    ('쌰', '쌰'),
    ('썌', '썌'),
    ('써', '써'),
    ('쎄', '쎄'),
    ('쎠', '쎠'),
    ('쎼', '쎼'),
    ('쏘', '쏘'),
    ('쏴', '쏴'),
    ('쐐', '쐐'),
    ('쐬', '쐬'),
    ('쑈', '쑈'),
    ('쑤', '쑤'),
    ('쒀', '쒀'),
    ('쒜', '쒜'),
    ('쒸', '쒸'),
    ('쓔', '쓔'),
    ('쓰', '쓰'),
    ('씌', '씌'),
    ('씨', '씨'),
    ('아', '아'),
    ('애', '애'),
    ('야', '야'),
    ('얘', '얘'),
    ('어', '어'),
    ('에', '에'),
    ('여', '여'),
    ('예', '예'),
    ('오', '오'),
    ('와', '와'),
    ('왜', '왜'),
    ('외', '외'),
    ('요', '요'),
    ('우', '우'),
    ('워', '워'),
    ('웨', '웨'),
    ('위', '위'),
    ('유', '유'),
    ('으', '으'),
    ('의', '의'),
    ('이', '이'),
    ('자', '자'),
    ('재', '재'),
    ('쟈', '쟈'),
    ('쟤', '쟤'),
    ('저', '저'),
    ('제', '제'),
    ('져', '져'),
    ('졔', '졔'),
    ('조', '조'),
    ('좌', '좌'),
    ('좨', '좨'),
    ('죄', '죄'),
    ('죠', '죠'),
    ('주', '주'),
    ('줘', '줘'),
    ('줴', '줴'),
    ('쥐', '쥐'),
    ('쥬', '쥬'),
    ('즈', '즈'),
    ('즤', '즤'),
    ('지', '지'),
    ('짜', '짜'),
    ('째', '째'),
    ('쨔', '쨔'),
    ('쨰', '쨰'),
    ('쩌', '쩌'),
    ('쩨', '쩨'),
    ('쪄', '쪄'),
    ('쪠', '쪠'),
    ('쪼', '쪼'),
    ('쫘', '쫘'),
    ('쫴', '쫴'),
    ('쬐', '쬐'),
    ('쬬', '쬬'),
    ('쭈', '쭈'),
    ('쭤', '쭤'),
    ('쮀', '쮀'),
    ('쮜', '쮜'),
    ('쮸', '쮸'),
    ('쯔', '쯔'),
    ('쯰', '쯰'),
    ('찌', '찌'),
    ('차', '차'),
    ('채', '채'),
    ('챠', '챠'),
    ('챼', '챼'),
    ('처', '처'),
    ('체', '체'),
    ('쳐', '쳐'),
    ('쳬', '쳬'),
    ('초', '초'),
    ('촤', '촤'),
    ('쵀', '쵀'),
    ('최', '최'),
    ('쵸', '쵸'),
    ('추', '추'),
    ('춰', '춰'),
    ('췌', '췌'),
    ('취', '취'),
    ('츄', '츄'),
    ('츠', '츠'),
    ('츼', '츼'),
    ('치', '치'),
    ('카', '카'),
    ('캐', '캐'),
    ('캬', '캬'),
    ('컈', '컈'),
    ('커', '커'),
    ('케', '케'),
    ('켜', '켜'),
    ('켸', '켸'),
    ('코', '코'),
    ('콰', '콰'),
    ('쾌', '쾌'),
    ('쾨', '쾨'),
    ('쿄', '쿄'),
    ('쿠', '쿠'),
    ('쿼', '쿼'),
    ('퀘', '퀘'),
    ('퀴', '퀴'),
    ('큐', '큐'),
    ('크', '크'),
    ('킈', '킈'),
    ('키', '키'),
    ('타', '타'),
    ('태', '태'),
    ('탸', '탸'),
    ('턔', '턔'),
    ('터', '터'),
    ('테', '테'),
    ('텨', '텨'),
    ('톄', '톄'),
    ('토', '토'),
    ('톼', '톼'),
    ('퇘', '퇘'),
    ('퇴', '퇴'),
    ('툐', '툐'),
    ('투', '투'),
    ('퉈', '퉈'),
    ('퉤', '퉤'),
    ('튀', '튀'),
    ('튜', '튜'),
    ('트', '트'),
    ('틔', '틔'),
    ('티', '티'),
    ('파', '파'),
    ('패', '패'),
    ('퍄', '퍄'),
    ('퍠', '퍠'),
    ('퍼', '퍼'),
    ('페', '페'),
    ('펴', '펴'),
    ('폐', '폐'),
    ('포', '포'),
    ('퐈', '퐈'),
    ('퐤', '퐤'),
    ('푀', '푀'),
    ('표', '표'),
    ('푸', '푸'),
    ('풔', '풔'),
    ('풰', '풰'),
    ('퓌', '퓌'),
    ('퓨', '퓨'),
    ('프', '프'),
    ('픠', '픠'),
    ('피', '피'),
    ('하', '하'),
    ('해', '해'),
    ('햐', '햐'),
    ('햬', '햬'),
    ('허', '허'),
    ('헤', '헤'),
    ('혀', '혀'),
    ('혜', '혜'),
    ('호', '호'),
    ('화', '화'),
    ('홰', '홰'),
    ('회', '회'),
    ('효', '효'),
    ('후', '후'),
    ('훠', '훠'),
    ('훼', '훼'),
    ('휘', '휘'),
    ('휴', '휴'),
    ('흐', '흐'),
    ('희', '희'),
    ('히', '히'),
];

pub const LVT: &'static [(char, char)] = &[
    ('각', '갛'),
    ('객', '갷'),
    ('갹', '걓'),
    ('걕', '걯'),
    ('걱', '겋'),
    ('겍', '겧'),
    ('격', '곃'),
    ('곅', '곟'),
    ('곡', '곻'),
    ('곽', '괗'),
    ('괙', '괳'),
    ('괵', '굏'),
    ('굑', '굫'),
    ('국', '궇'),
    ('궉', '궣'),
    ('궥', '궿'),
    ('귁', '귛'),
    ('귝', '귷'),
    ('극', '긓'),
    ('긕', '긯'),
    ('긱', '깋'),
    ('깍', '깧'),
    ('깩', '꺃'),
    ('꺅', '꺟'),
    ('꺡', '꺻'),
    ('꺽', '껗'),
    ('껙', '껳'),
    ('껵', '꼏'),
    ('꼑', '꼫'),
    ('꼭', '꽇'),
    ('꽉', '꽣'),
    ('꽥', '꽿'),
    ('꾁', '꾛'),
    ('꾝', '꾷'),
    ('꾹', '꿓'),
    ('꿕', '꿯'),
    ('꿱', '뀋'),
    ('뀍', '뀧'),
    ('뀩', '끃'),
    ('끅', '끟'),
    ('끡', '끻'),
    ('끽', '낗'),
    ('낙', '낳'),
    ('낵', '냏'),
    ('냑', '냫'),
    ('냭', '넇'),
    ('넉', '넣'),
    ('넥', '넿'),
    ('녁', '녛'),
    ('녝', '녷'),
    ('녹', '놓'),
    ('놕', '놯'),
    ('놱', '뇋'),
    ('뇍', '뇧'),
    ('뇩', '눃'),
    ('눅', '눟'),
    ('눡', '눻'),
    ('눽', '뉗'),
    ('뉙', '뉳'),
    ('뉵', '늏'),
    ('늑', '늫'),
    ('늭', '닇'),
    ('닉', '닣'),
    ('닥', '닿'),
    ('댁', '댛'),
    ('댝', '댷'),
    ('댹', '덓'),
    ('덕', '덯'),
    ('덱', '뎋'),
    ('뎍', '뎧'),
    ('뎩', '돃'),
    ('독', '돟'),
    ('돡', '돻'),
    ('돽', '됗'),
    ('됙', '됳'),
    ('됵', '둏'),
    ('둑', '둫'),
    ('둭', '뒇'),
    ('뒉', '뒣'),
    ('뒥', '뒿'),
    ('듁', '듛'),
    ('득', '듷'),
    ('듹', '딓'),
    ('딕', '딯'),
    ('딱', '땋'),
    ('땍', '땧'),
    ('땩', '떃'),
    ('떅', '떟'),
    ('떡', '떻'),
    ('떽', '뗗'),
    ('뗙', '뗳'),
    ('뗵', '똏'),
    ('똑', '똫'),
    ('똭', '뙇'),
    ('뙉', '뙣'),
    ('뙥', '뙿'),
    ('뚁', '뚛'),
    ('뚝', '뚷'),
    ('뚹', '뛓'),
    ('뛕', '뛯'),
    ('뛱', '뜋'),
    ('뜍', '뜧'),
    ('뜩', '띃'),
    ('띅', '띟'),
    ('띡', '띻'),
    ('락', '랗'),
    ('랙', '랳'),
    ('략', '럏'),
    ('럑', '럫'),
    ('럭', '렇'),
    ('렉', '렣'),
    ('력', '렿'),
    ('롁', '롛'),
    ('록', '롷'),
    ('롹', '뢓'),
    ('뢕', '뢯'),
    ('뢱', '룋'),
    ('룍', '룧'),
    ('룩', '뤃'),
    ('뤅', '뤟'),
    ('뤡', '뤻'),
    ('뤽', '륗'),
    ('륙', '륳'),
    ('륵', '릏'),
    ('릑', '릫'),
    ('릭', '맇'),
    ('막', '맣'),
    ('맥', '맿'),
    ('먁', '먛'),
    ('먝', '먷'),
    ('먹', '멓'),
    ('멕', '멯'),
    ('멱', '몋'),
    ('몍', '몧'),
    ('목', '뫃'),
    ('뫅', '뫟'),
    ('뫡', '뫻'),
    ('뫽', '묗'),
    ('묙', '묳'),
    ('묵', '뭏'),
    ('뭑', '뭫'),
    ('뭭', '뮇'),
    ('뮉', '뮣'),
    ('뮥', '뮿'),
    ('믁', '믛'),
    ('믝', '믷'),
    ('믹', '밓'),
    ('박', '밯'),
    ('백', '뱋'),
    ('뱍', '뱧'),
    ('뱩', '벃'),
    ('벅', '벟'),
    ('벡', '벻'),
    ('벽', '볗'),
    ('볙', '볳'),
    ('복', '봏'),
    ('봑', '봫'),
    ('봭', '뵇'),
    ('뵉', '뵣'),
    ('뵥', '뵿'),
    ('북', '붛'),
    ('붝', '붷'),
    ('붹', '뷓'),
    ('뷕', '뷯'),
    ('뷱', '븋'),
    ('븍', '븧'),
    ('븩', '빃'),
    ('빅', '빟'),
    ('빡', '빻'),
    ('빽', '뺗'),
    ('뺙', '뺳'),
    ('뺵', '뻏'),
    ('뻑', '뻫'),
    ('뻭', '뼇'),
    ('뼉', '뼣'),
    ('뼥', '뼿'),
    ('뽁', '뽛'),
    ('뽝', '뽷'),
    ('뽹', '뾓'),
    ('뾕', '뾯'),
    ('뾱', '뿋'),
    ('뿍', '뿧'),
    ('뿩', '쀃'),
    ('쀅', '쀟'),
    ('쀡', '쀻'),
    ('쀽', '쁗'),
    ('쁙', '쁳'),
    ('쁵', '삏'),
    ('삑', '삫'),
    ('삭', '샇'),
    ('색', '샣'),
    ('샥', '샿'),
    ('섁', '섛'),
    ('석', '섷'),
    ('섹', '셓'),
    ('셕', '셯'),
    ('셱', '솋'),
    ('속', '솧'),
    ('솩', '쇃'),
    ('쇅', '쇟'),
    ('쇡', '쇻'),
    ('쇽', '숗'),
    ('숙', '숳'),
    ('숵', '쉏'),
    ('쉑', '쉫'),
    ('쉭', '슇'),
    ('슉', '슣'),
    ('슥', '슿'),
    ('싁', '싛'),
    ('식', '싷'),
    ('싹', '쌓'),
    ('쌕', '쌯'),
    ('쌱', '썋'),
    ('썍', '썧'),
    ('썩', '쎃'),
    ('쎅', '쎟'),
    ('쎡', '쎻'),
    ('쎽', '쏗'),
    ('쏙', '쏳'),
    ('쏵', '쐏'),
    ('쐑', '쐫'),
    ('쐭', '쑇'),
    ('쑉', '쑣'),
    ('쑥', '쑿'),
    ('쒁', '쒛'),
    ('쒝', '쒷'),
    ('쒹', '쓓'),
    ('쓕', '쓯'),
    ('쓱', '씋'),
    ('씍', '씧'),
    ('씩', '앃'),
    ('악', '앟'),
    ('액', '앻'),
    ('약', '얗'),
    ('얙', '얳'),
    ('억', '엏'),
    ('엑', '엫'),
    ('역', '옇'),
    ('옉', '옣'),
    ('옥', '옿'),
    ('왁', '왛'),
    ('왝', '왷'),
    ('왹', '욓'),
    ('욕', '욯'),
    ('욱', '웋'),
    ('웍', '웧'),
    ('웩', '윃'),
    ('윅', '윟'),
    ('육', '윻'),
    ('윽', '읗'),
    ('읙', '읳'),
    ('익', '잏'),
    ('작', '잫'),
    ('잭', '쟇'),
    ('쟉', '쟣'),
    ('쟥', '쟿'),
    ('적', '젛'),
    ('젝', '젷'),
    ('젹', '졓'),
    ('졕', '졯'),
    ('족', '좋'),
    ('좍', '좧'),
    ('좩', '죃'),
    ('죅', '죟'),
    ('죡', '죻'),
    ('죽', '줗'),
    ('줙', '줳'),
    ('줵', '쥏'),
    ('쥑', '쥫'),
    ('쥭', '즇'),
    ('즉', '즣'),
    ('즥', '즿'),
    ('직', '짛'),
    ('짝', '짷'),
    ('짹', '쨓'),
    ('쨕', '쨯'),
    ('쨱', '쩋'),
    ('쩍', '쩧'),
    ('쩩', '쪃'),
    ('쪅', '쪟'),
    ('쪡', '쪻'),
    ('쪽', '쫗'),
    ('쫙', '쫳'),
    ('쫵', '쬏'),
    ('쬑', '쬫'),
    ('쬭', '쭇'),
    ('쭉', '쭣'),
    ('쭥', '쭿'),
    ('쮁', '쮛'),
    ('쮝', '쮷'),
    ('쮹', '쯓'),
    ('쯕', '쯯'),
    ('쯱', '찋'),
    ('찍', '찧'),
    ('착', '챃'),
    ('책', '챟'),
    ('챡', '챻'),
    ('챽', '첗'),
    ('척', '첳'),
    ('첵', '쳏'),
    ('쳑', '쳫'),
    ('쳭', '촇'),
    ('촉', '촣'),
    ('촥', '촿'),
    ('쵁', '쵛'),
    ('쵝', '쵷'),
    ('쵹', '춓'),
    ('축', '춯'),
    ('춱', '췋'),
    ('췍', '췧'),
    ('췩', '츃'),
    ('츅', '츟'),
    ('측', '츻'),
    ('츽', '칗'),
    ('칙', '칳'),
    ('칵', '캏'),
    ('캑', '캫'),
    ('캭', '컇'),
    ('컉', '컣'),
    ('컥', '컿'),
    ('켁', '켛'),
    ('켝', '켷'),
    ('켹', '콓'),
    ('콕', '콯'),
    ('콱', '쾋'),
    ('쾍', '쾧'),
    ('쾩', '쿃'),
    ('쿅', '쿟'),
    ('쿡', '쿻'),
    ('쿽', '퀗'),
    ('퀙', '퀳'),
    ('퀵', '큏'),
    ('큑', '큫'),
    ('큭', '킇'),
    ('킉', '킣'),
    ('킥', '킿'),
    ('탁', '탛'),
    ('택', '탷'),
    ('탹', '턓'),
    ('턕', '턯'),
    ('턱', '텋'),
    ('텍', '텧'),
    ('텩', '톃'),
    ('톅', '톟'),
    ('톡', '톻'),
    ('톽', '퇗'),
    ('퇙', '퇳'),
    ('퇵', '툏'),
    ('툑', '툫'),
    ('툭', '퉇'),
    ('퉉', '퉣'),
    ('퉥', '퉿'),
    ('튁', '튛'),
    ('튝', '튷'),
    ('특', '틓'),
    ('틕', '틯'),
    ('틱', '팋'),
    ('팍', '팧'),
    ('팩', '퍃'),
    ('퍅', '퍟'),
    ('퍡', '퍻'),
    ('퍽', '펗'),
    ('펙', '펳'),
    ('펵', '폏'),
    ('폑', '폫'),
    ('폭', '퐇'),
    ('퐉', '퐣'),
    ('퐥', '퐿'),
    ('푁', '푛'),
    ('푝', '푷'),
    ('푹', '풓'),
    ('풕', '풯'),
    ('풱', '퓋'),
    ('퓍', '퓧'),
    ('퓩', '픃'),
    ('픅', '픟'),
    ('픡', '픻'),
    ('픽', '핗'),
    ('학', '핳'),
    ('핵', '햏'),
    ('햑', '햫'),
    ('햭', '헇'),
    ('헉', '헣'),
    ('헥', '헿'),
    ('혁', '혛'),
    ('혝', '혷'),
    ('혹', '홓'),
    ('확', '홯'),
    ('홱', '횋'),
    ('획', '횧'),
    ('횩', '훃'),
    ('훅', '훟'),
    ('훡', '훻'),
    ('훽', '휗'),
    ('휙', '휳'),
    ('휵', '흏'),
    ('흑', '흫'),
    ('흭', '힇'),
    ('힉', '힣'),
];

pub const PREPEND: &'static [(char, char)] = &[
    ('\u{600}', '\u{605}'),
    ('\u{6dd}', '\u{6dd}'),
    ('\u{70f}', '\u{70f}'),
    ('\u{890}', '\u{891}'),
    ('\u{8e2}', '\u{8e2}'),
    ('ൎ', 'ൎ'),
    ('\u{110bd}', '\u{110bd}'),
    ('\u{110cd}', '\u{110cd}'),
    ('𑇂', '𑇃'),
    ('𑤿', '𑤿'),
    ('𑥁', '𑥁'),
    ('𑨺', '𑨺'),
    ('𑪄', '𑪉'),
    ('𑵆', '𑵆'),
    ('𑼂', '𑼂'),
];

pub const REGIONAL_INDICATOR: &'static [(char, char)] = &[('🇦', '🇿')];

pub const SPACINGMARK: &'static [(char, char)] = &[
    ('ः', 'ः'),
    ('ऻ', 'ऻ'),
    ('ा', 'ी'),
    ('ॉ', 'ौ'),
    ('ॎ', 'ॏ'),
    ('ং', 'ঃ'),
    ('ি', 'ী'),
    ('ে', 'ৈ'),
    ('ো', 'ৌ'),
    ('ਃ', 'ਃ'),
    ('ਾ', 'ੀ'),
    ('ઃ', 'ઃ'),
    ('ા', 'ી'),
    ('ૉ', 'ૉ'),
    ('ો', 'ૌ'),
    ('ଂ', 'ଃ'),
    ('ୀ', 'ୀ'),
    ('େ', 'ୈ'),
    ('ୋ', 'ୌ'),
    ('ி', 'ி'),
    ('ு', 'ூ'),
    ('ெ', 'ை'),
    ('ொ', 'ௌ'),
    ('ఁ', 'ః'),
    ('ు', 'ౄ'),
    ('ಂ', 'ಃ'),
    ('ಾ', 'ಾ'),
    ('ೀ', 'ು'),
    ('ೃ', 'ೄ'),
    ('ೇ', 'ೈ'),
    ('ೊ', 'ೋ'),
    ('ೳ', 'ೳ'),
    ('ം', 'ഃ'),
    ('ി', 'ീ'),
    ('െ', 'ൈ'),
    ('ൊ', 'ൌ'),
    ('ං', 'ඃ'),
    ('ැ', 'ෑ'),
    ('ෘ', 'ෞ'),
    ('ෲ', 'ෳ'),
    ('ำ', 'ำ'),
    ('ຳ', 'ຳ'),
    ('༾', '༿'),
    ('ཿ', 'ཿ'),
    ('ေ', 'ေ'),
    ('ျ', 'ြ'),
    ('ၖ', 'ၗ'),
    ('ႄ', 'ႄ'),
    ('᜕', '᜕'),
    ('᜴', '᜴'),
    ('ា', 'ា'),
    ('ើ', 'ៅ'),
    ('ះ', 'ៈ'),
    ('ᤣ', 'ᤦ'),
    ('ᤩ', 'ᤫ'),
    ('ᤰ', 'ᤱ'),
    ('ᤳ', 'ᤸ'),
    ('ᨙ', 'ᨚ'),
    ('ᩕ', 'ᩕ'),
    ('ᩗ', 'ᩗ'),
    ('ᩭ', 'ᩲ'),
    ('ᬄ', 'ᬄ'),
    ('ᬻ', 'ᬻ'),
    ('ᬽ', 'ᭁ'),
    ('ᭃ', '᭄'),
    ('ᮂ', 'ᮂ'),
    ('ᮡ', 'ᮡ'),
    ('ᮦ', 'ᮧ'),
    ('᮪', '᮪'),
    ('ᯧ', 'ᯧ'),
    ('ᯪ', 'ᯬ'),
    ('ᯮ', 'ᯮ'),
    ('᯲', '᯳'),
    ('ᰤ', 'ᰫ'),
    ('ᰴ', 'ᰵ'),
    ('᳡', '᳡'),
    ('᳷', '᳷'),
    ('ꠣ', 'ꠤ'),
    ('ꠧ', 'ꠧ'),
    ('ꢀ', 'ꢁ'),
    ('ꢴ', 'ꣃ'),
    ('ꥒ', '꥓'),
    ('ꦃ', 'ꦃ'),
    ('ꦴ', 'ꦵ'),
    ('ꦺ', 'ꦻ'),
    ('ꦾ', '꧀'),
    ('ꨯ', 'ꨰ'),
    ('ꨳ', 'ꨴ'),
    ('ꩍ', 'ꩍ'),
    ('ꫫ', 'ꫫ'),
    ('ꫮ', 'ꫯ'),
    ('ꫵ', 'ꫵ'),
    ('ꯣ', 'ꯤ'),
    ('ꯦ', 'ꯧ'),
    ('ꯩ', 'ꯪ'),
    ('꯬', '꯬'),
    ('𑀀', '𑀀'),
    ('𑀂', '𑀂'),
    ('𑂂', '𑂂'),
    ('𑂰', '𑂲'),
    ('𑂷', '𑂸'),
    ('𑄬', '𑄬'),
    ('𑅅', '𑅆'),
    ('𑆂', '𑆂'),
    ('𑆳', '𑆵'),
    ('𑆿', '𑇀'),
    ('𑇎', '𑇎'),
    ('𑈬', '𑈮'),
    ('𑈲', '𑈳'),
    ('𑈵', '𑈵'),
    ('𑋠', '𑋢'),
    ('𑌂', '𑌃'),
    ('𑌿', '𑌿'),
    ('𑍁', '𑍄'),
    ('𑍇', '𑍈'),
    ('𑍋', '𑍍'),
    ('𑍢', '𑍣'),
    ('𑐵', '𑐷'),
    ('𑑀', '𑑁'),
    ('𑑅', '𑑅'),
    ('𑒱', '𑒲'),
    ('𑒹', '𑒹'),
    ('𑒻', '𑒼'),
    ('𑒾', '𑒾'),
    ('𑓁', '𑓁'),
    ('𑖰', '𑖱'),
    ('𑖸', '𑖻'),
    ('𑖾', '𑖾'),
    ('𑘰', '𑘲'),
    ('𑘻', '𑘼'),
    ('𑘾', '𑘾'),
    ('𑚬', '𑚬'),
    ('𑚮', '𑚯'),
    ('𑚶', '𑚶'),
    ('𑜦', '𑜦'),
    ('𑠬', '𑠮'),
    ('𑠸', '𑠸'),
    ('𑤱', '𑤵'),
    ('𑤷', '𑤸'),
    ('𑤽', '𑤽'),
    ('𑥀', '𑥀'),
    ('𑥂', '𑥂'),
    ('𑧑', '𑧓'),
    ('𑧜', '𑧟'),
    ('𑧤', '𑧤'),
    ('𑨹', '𑨹'),
    ('𑩗', '𑩘'),
    ('𑪗', '𑪗'),
    ('𑰯', '𑰯'),
    ('𑰾', '𑰾'),
    ('𑲩', '𑲩'),
    ('𑲱', '𑲱'),
    ('𑲴', '𑲴'),
    ('𑶊', '𑶎'),
    ('𑶓', '𑶔'),
    ('𑶖', '𑶖'),
    ('𑻵', '𑻶'),
    ('𑼃', '𑼃'),
    ('𑼴', '𑼵'),
    ('𑼾', '𑼿'),
    ('𑽁', '𑽁'),
    ('𖽑', '𖾇'),
    ('𖿰', '𖿱'),
    ('𝅦', '𝅦'),
    ('𝅭', '𝅭'),
];

pub const T: &'static [(char, char)] = &[('ᆨ', 'ᇿ'), ('ퟋ', 'ퟻ')];

pub const V: &'static [(char, char)] = &[('ᅠ', 'ᆧ'), ('ힰ', 'ퟆ')];

pub const ZWJ: &'static [(char, char)] = &[('\u{200d}', '\u{200d}')];
