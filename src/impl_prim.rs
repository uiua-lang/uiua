//! All primitive definitions

use std::fmt;

use serde::*;

use crate::{algorithm::ga, Primitive, Purity, SubSide, Subscript, SysOp, SUBSCRIPT_DIGITS};

macro_rules! impl_primitive {
    ($(
        $(#[$attr:meta])*
        (
            $($args:literal)?
            $(($outputs:expr))?
            $([$margs:expr])?,
            $variant:ident $(($($inner:ty),* $(,)?))?
            $(, $purity:ident)?
            $(,{ga: $ga:literal})?
        )
    ),* $(,)?) => {
        /// Primitives that exist as an implementation detail
        #[doc(hidden)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
        pub enum ImplPrimitive {
            $(
                $(#[$attr])*
                $variant $(($($inner),*))?,
            )*
            UndoDeshape(Option<i32>),
            EachSub(i32),
            RowsSub(Subscript<i32>, bool),
            UndoTransposeN(usize, i32),
            UndoReverse { n: usize, all: bool },
            UndoRotate(usize),
            ReduceDepth(usize),
            StackN { n: usize, inverse: bool },
            OnSub(usize),
            BySub(usize),
            WithSub(usize),
            OffSub(usize),
            SidedFill(SubSide),
            SidedEncodeBytes(SubSide),
            DecodeBytes(Option<SubSide>),
            /// Push the maximum row count of N values
            MaxRowCount(usize),
            BothImpl(Subscript<u32>),
            UnBothImpl(Subscript<u32>),
            Ga(ga::GaOp, ga::Spec),
        }

        impl ImplPrimitive {
            pub fn args(&self) -> Option<usize> {
                Some(match self {
                    $($(ImplPrimitive::$variant { .. }  => $args,)?)*
                    ImplPrimitive::UndoDeshape(_) => 2,
                    ImplPrimitive::UndoTransposeN(n, _) => *n,
                    ImplPrimitive::UndoReverse { n, .. } => *n,
                    ImplPrimitive::UndoRotate(n) => *n + 1,
                    ImplPrimitive::ReduceDepth(_) => 1,
                    ImplPrimitive::StackN { n, .. } => *n,
                    ImplPrimitive::MaxRowCount(n) => *n,
                    ImplPrimitive::SidedEncodeBytes(_) | ImplPrimitive::DecodeBytes(_) => 2,
                    ImplPrimitive::Ga(op, _) => op.args(),
                    _ => return None
                })
            }
            pub fn outputs(&self) -> Option<usize> {
                Some(match self {
                    $($(ImplPrimitive::$variant { .. } => $outputs,)?)*
                    ImplPrimitive::UndoTransposeN(n, _) => *n,
                    ImplPrimitive::UndoReverse { n, .. } => *n,
                    ImplPrimitive::UndoRotate(n) => *n,
                    ImplPrimitive::StackN { n, .. } => *n,
                    ImplPrimitive::MaxRowCount(n) => *n + 1,
                    ImplPrimitive::SidedEncodeBytes(_) | ImplPrimitive::DecodeBytes(_) => 1,
                    ImplPrimitive::Ga(op, _) => op.outputs(),
                    _ if self.modifier_args().is_some() => return None,
                    _ => 1
                })
            }
            pub fn modifier_args(&self) -> Option<usize> {
                match self {
                    $($(ImplPrimitive::$variant { .. } => Some($margs),)?)*
                    ImplPrimitive::ReduceDepth(_)
                    | ImplPrimitive::EachSub(_)
                    | ImplPrimitive::RowsSub(..) => Some(1),
                    ImplPrimitive::OnSub(_)
                    | ImplPrimitive::BySub(_)
                    | ImplPrimitive::WithSub(_)
                    | ImplPrimitive::OffSub(_)
                    | ImplPrimitive::BothImpl(_)
                    | ImplPrimitive::UnBothImpl(_) => Some(1),
                    ImplPrimitive::SidedFill(_) => Some(2),
                    _ => None
                }
            }
            pub fn purity(&self) -> Purity {
                match self {
                    $($(ImplPrimitive::$variant => {Purity::$purity},)*)*
                    ImplPrimitive::StackN { .. } => Purity::Mutating,
                    _ => Purity::Pure
                }
            }
        }
    };
}

impl_primitive!(
    // Inverses
    (2, Root),
    (1, Cos),
    (1, Asin),
    (1, Acos),
    (1, Ln),
    (0, UnPop, Impure),
    (1, UnBits),
    (1, UnWhere),
    (1(2), UnCouple),
    (1(2), UnAdd),
    (1(2), UnMul),
    (1(2), UnDiv),
    (1(2), UnAtan),
    (1(2), UnComplex),
    (1, UnUtf8),
    (1, UnUtf16),
    (1, UnGraphemes),
    (1, UnParse),
    (1, UnParseSub(usize)),
    (1, UnFix),
    (1, UnShape),
    (1[1], UnScan),
    (1(2), UnMap),
    (0(0), UnStack, Impure),
    (0(0)[1], UnDump, Impure),
    (0[2], UnFill),
    (1, Primes),
    (1, UnBox),
    (2, AntiDrop),
    (2, AntiSelect),
    (2, AntiPick),
    (2, AntiKeep),
    (2, AntiRotate),
    (1(2), UnJoin),
    (1(2), UnJoinEnd),
    (2(2), UnJoinShape),
    (2(2), UnJoinShapeEnd),
    (3(2), UnJoinShape2),
    (3(2), UnJoinShape2End),
    (1(2), UnKeep),
    (1(2), UnTake),
    (1(2)[1], UnGroup),
    (1(2)[1], UnPartition),
    (1, UnSort, Impure),
    (1, UnHsv),
    (1, UnJson),
    (1, UnBinary),
    (1(2), UnCompress),
    (2, AntiCompress),
    (1, UnCsv),
    (1, UnXlsx),
    (1, UnFft),
    (1, UnDatetime),
    (2(0), MatchPattern),
    (2, MatchLe),
    (2, MatchGe),
    (1(2), ImageDecode),
    (1(2), GifDecode),
    (1(3), AudioDecode),
    (0, UnRawMode, Impure),
    (0, UnChangeDirectory, Impure),
    (1(0), UnClip, Mutating),
    // Unders
    (1, UndoFix),
    (2, UndoShape),
    (2, UndoUnBits),
    (2, AntiBase),
    (3, UndoSelect),
    (3, UndoPick),
    (3, UndoTake),
    (3, UndoDrop),
    (2, UndoFirst),
    (2, UndoLast),
    (3, UndoKeep),
    (3, UndoRerank),
    (2, UndoReshape),
    (2, UndoWindows),
    (2, UndoWhere),
    (2, AntiOrient),
    (3, UndoAntiOrient),
    (2(2), DoRegex),
    (4, UndoRegex),
    (3(2), UndoJoin),
    (1(1)[1], UndoPartition1),
    (3, UndoPartition2),
    (1(1)[1], UndoGroup1),
    (3, UndoGroup2),
    (3, UndoGet),
    (4, UndoInsert),
    (3, UndoRemove),
    (1(0), TryClose),
    ([2], UnBracket),
    ([1], UndoRows),
    ([1], UndoInventory),
    (2, SetSign),
    // Optimizations
    (1, FirstMinIndex),
    (1, FirstMaxIndex),
    (1, LastMinIndex),
    (1, LastMaxIndex),
    (1, FirstWhere),
    (1, LastWhere),
    (1, LenWhere),
    (2, MemberOfRange),
    (2, MultidimMemberOfRange),
    (1, RandomRow, Impure),
    (1, SortDown),
    (1, AllSame),
    (1, OneUnique),
    (1[1], ReduceContent),
    ([1], ReduceConjoinInventory),
    (2(1)[2], ReduceTable),
    (1, ReplaceRand, Impure),
    (2, ReplaceRand2, Impure),
    (1, CountUnique),
    ((2)[3], Astar),
    ((2)[3], AstarFirst),
    ((2)[3], AstarSignLen),
    ((1)[3], AstarPop),
    ((1)[3], AstarTake),
    ((2)[2], PathFirst),
    ((2)[3], PathSignLen),
    ((1)[2], PathPop),
    ((1)[2], PathTake),
    (2[1], SplitByScalar),
    (2[1], SplitBy),
    (2[1], SplitByKeepEmpty),
    (2, AbsComplex),
    (2, MatrixDiv),
    (2, RangeStart),
    // Implementation details
    (2(3), Over),
    ([1], DipN(usize)),
    (1, NBits(usize)),
    (1, DeshapeSub(i32)),
    (1, ClassifySub(usize)),
    (1, ParseSub(usize)),
    (1, TransposeN(i32)),
    (2, MultiKeep(usize)),
    (2, SidedJoin(SubSide)),
    (1, Utf16),
    (1, Retropose),
    ([1], FixMatchRanks),
    ([2], RepeatWithInverse),
    ([1], RepeatCountConvergence),
    (2(1), ValidateType),
    (2(0), ValidateTypeConsume),
    (2(0), TestAssert, Impure),
    /// Validate that a non-boxed variant field has a valid type and rank
    (1, ValidateNonBoxedVariant),
    (2(1), ValidateVariant),
    (2(1), TagVariant),
    (3, LayoutArgs),
    (2, VoxelsArgs),
);

fn fmt_subscript(f: &mut fmt::Formatter<'_>, i: i32) -> fmt::Result {
    write!(f, "{}", Subscript::numeric(i))
}

impl fmt::Display for ImplPrimitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ImplPrimitive::*;
        use Primitive::*;
        match self {
            &DeshapeSub(i) => {
                write!(f, "{Deshape}")?;
                fmt_subscript(f, i)
            }
            &ClassifySub(i) => {
                write!(f, "{Classify}")?;
                fmt_subscript(f, i as i32)
            }
            &ParseSub(i) => {
                write!(f, "{Parse}")?;
                fmt_subscript(f, i as i32)
            }
            &UnParseSub(i) => {
                write!(f, "{Un}{Parse}")?;
                fmt_subscript(f, i as i32)
            }
            &EachSub(i) => {
                write!(f, "{Each}")?;
                fmt_subscript(f, i)
            }
            &RowsSub(sub, inv) => {
                if inv {
                    write!(f, "{Inventory}")?;
                } else {
                    write!(f, "{Rows}")?;
                }
                sub.fmt(f)
            }
            OnSub(i) => {
                write!(f, "{On}")?;
                fmt_subscript(f, *i as i32)
            }
            BySub(i) => {
                write!(f, "{By}")?;
                fmt_subscript(f, *i as i32)
            }
            WithSub(i) => {
                write!(f, "{With}")?;
                fmt_subscript(f, *i as i32)
            }
            OffSub(i) => {
                write!(f, "{Off}")?;
                fmt_subscript(f, *i as i32)
            }
            NBits(n) => {
                write!(f, "{Bits}")?;
                fmt_subscript(f, *n as i32)
            }
            SidedEncodeBytes(side) => write!(f, "{EncodeBytes}{side}"),
            Root => write!(f, "{Anti}{Pow}"),
            Cos => write!(f, "cos"),
            Asin => write!(f, "{Un}{Sin}"),
            Acos => write!(f, "{Un}{Cos}"),
            Ln => write!(f, "{Un}{Exp}"),
            UnPop => write!(f, "{Un}{Pop}"),
            UnBits => write!(f, "{Un}{Bits}"),
            UnWhere => write!(f, "{Un}{Where}"),
            UnCouple => write!(f, "{Un}{Couple}"),
            UnMap => write!(f, "{Un}{Map}"),
            UnAtan => write!(f, "{Un}{Atan}"),
            UnComplex => write!(f, "{Un}{Complex}"),
            UnAdd => write!(f, "{Un}{Add}"),
            UnMul => write!(f, "{Un}{Mul}"),
            UnDiv => write!(f, "{Un}{Div}"),
            UnUtf8 => write!(f, "{Un}{Utf8}"),
            UnUtf16 => write!(f, "{Un}{Utf16}"),
            Utf16 => write!(f, "utf₁₆"),
            UnGraphemes => write!(f, "{Un}{Graphemes}"),
            UnParse => write!(f, "{Un}{Parse}"),
            UnFix => write!(f, "{Un}{Fix}"),
            UnShape => write!(f, "{Un}{Shape}"),
            AntiDrop => write!(f, "{Anti}{Drop}"),
            AntiSelect => write!(f, "{Anti}{Select}"),
            AntiPick => write!(f, "{Anti}{Pick}"),
            AntiKeep => write!(f, "{Anti}{Keep}"),
            AntiRotate => write!(f, "{Anti}{Rotate}"),
            UnJoin | UnJoinShape | UnJoinShape2 => write!(f, "{Un}{Join}"),
            UnJoinEnd | UnJoinShapeEnd | UnJoinShape2End => write!(f, "{Un}{Backward}{Join}"),
            UnKeep => write!(f, "{Un}{Keep}"),
            UnTake => write!(f, "{Un}{Take}"),
            UnScan => write!(f, "{Un}{Scan}"),
            UnGroup => write!(f, "{Un}{Group}"),
            UnPartition => write!(f, "{Un}{Partition}"),
            UnStack => write!(f, "{Un}{Args}"),
            UnDump => write!(f, "{Un}{Dump}"),
            UnFill => write!(f, "{Un}{Fill}"),
            UnBox => write!(f, "{Un}{Box}"),
            UnSort => write!(f, "{Un}{Sort}"),
            UnHsv => write!(f, "{Un}{Hsv}"),
            UnJson => write!(f, "{Un}{Json}"),
            UnBinary => write!(f, "{Un}{Binary}"),
            UnCompress => write!(f, "{Un}{Compress}"),
            AntiCompress => write!(f, "{Anti}{Compress}"),
            UnCsv => write!(f, "{Un}{Csv}"),
            UnXlsx => write!(f, "{Un}{Xlsx}"),
            UnFft => write!(f, "{Un}{Fft}"),
            UnDatetime => write!(f, "{Un}{DateTime}"),
            UnBracket => write!(f, "{Un}{Bracket}"),
            DecodeBytes(Some(side)) => write!(f, "{Un}{EncodeBytes}{side}"),
            DecodeBytes(None) => write!(f, "{Un}{EncodeBytes}"),
            ImageDecode => write!(f, "{Un}{ImageEncode}"),
            GifDecode => write!(f, "{Un}{GifEncode}"),
            AudioDecode => write!(f, "{Un}{AudioEncode}"),
            UnRawMode => write!(f, "{Un}{}", Sys(SysOp::RawMode)),
            UnChangeDirectory => write!(f, "{Un}{}", Sys(SysOp::ChangeDirectory)),
            UnClip => write!(f, "{Un}{}", Sys(SysOp::Clip)),
            UndoUnBits => write!(f, "{Under}{Un}{Bits}"),
            AntiBase => write!(f, "{Anti}{Base}"),
            UndoReverse { n, .. } => write!(f, "{Under}{Reverse}({n})"),
            UndoTransposeN(n, _) => write!(f, "{Under}{Transpose}({n})"),
            UndoRotate(n) => write!(f, "{Under}{Rotate}({n})"),
            UndoTake => write!(f, "{Under}{Take}"),
            UndoDrop => write!(f, "{Under}{Drop}"),
            UndoSelect => write!(f, "{Under}{Select}"),
            UndoPick => write!(f, "{Under}{Pick}"),
            UndoWhere => write!(f, "{Under}{Where}"),
            AntiOrient => write!(f, "{Anti}{Orient}"),
            UndoAntiOrient => write!(f, "{Under}{Orient}"),
            DoRegex => write!(f, "{Regex}"),
            UndoRegex => write!(f, "{Under}{Regex}"),
            UndoGet => write!(f, "{Under}{Get}"),
            UndoInsert => write!(f, "{Under}{Insert}"),
            UndoRemove => write!(f, "{Under}{Remove}"),
            UndoPartition1 | UndoPartition2 => write!(f, "{Under}{Partition}"),
            UndoGroup1 | UndoGroup2 => write!(f, "{Under}{Group}"),
            TryClose => write!(f, "{}", Sys(SysOp::Close)),
            UndoFix => write!(f, "{Under}{Fix}"),
            UndoShape => write!(f, "{Under}{Shape}"),
            UndoDeshape(_) => write!(f, "{Under}{Deshape}"),
            UndoFirst => write!(f, "{Under}{First}"),
            UndoLast => write!(f, "{Under}{Last}"),
            UndoKeep => write!(f, "{Under}{Keep}"),
            UndoRerank => write!(f, "{Under}{Rerank}"),
            UndoReshape => write!(f, "{Un}{Reshape}"),
            UndoWindows => write!(f, "{Un}{Stencil}{Identity}"),
            UndoJoin => write!(f, "{Under}{Join}"),
            UndoRows => write!(f, "{Under}{Rows}"),
            UndoInventory => write!(f, "{Under}{Inventory}"),
            MaxRowCount(n) => write!(f, "MaxRowCount({n})"),
            SetSign => write!(f, "{Under}{Sign}"),
            // Optimizations
            FirstMinIndex => write!(f, "{First}{Rise}"),
            FirstMaxIndex => write!(f, "{First}{Fall}"),
            LastMinIndex => write!(f, "{First}{Reverse}{Rise}"),
            LastMaxIndex => write!(f, "{First}{Reverse}{Fall}"),
            FirstWhere => write!(f, "{First}{Where}"),
            LastWhere => write!(f, "{First}{Reverse}{Where}"),
            LenWhere => write!(f, "{Len}{Where}"),
            MemberOfRange => write!(f, "{MemberOf}{Range}"),
            MultidimMemberOfRange => write!(f, "{MemberOf}{Rerank}1{Range}"),
            RandomRow => write!(f, "{First}{Un}{Sort}"),
            SortDown => write!(f, "{Select}{Fall}{Dup}"),
            AllSame => write!(f, "all same"),
            OneUnique => write!(f, "{Eq}1{Len}{Deduplicate}"),
            Primes => write!(f, "{Un}{Reduce}{Mul}"),
            ReplaceRand => write!(f, "{Gap}{Rand}"),
            ReplaceRand2 => write!(f, "{Gap}{Gap}{Rand}"),
            ReduceContent => write!(f, "{Reduce}{Content}"),
            ReduceConjoinInventory => write!(f, "{Reduce}{Content}{Join}{Inventory}"),
            ReduceTable => write!(f, "{Reduce}(…){Table}"),
            CountUnique => write!(f, "{Len}{Deduplicate}"),
            MatchPattern => write!(f, "pattern match"),
            MatchLe => write!(f, "match ≤"),
            MatchGe => write!(f, "match ≥"),
            Astar => write!(f, "{Path}"),
            AstarFirst => write!(f, "{First}{Astar}"),
            AstarSignLen => write!(f, "{Sign}{Len}{Astar}"),
            AstarTake => write!(f, "{Take}…{Astar}"),
            AstarPop => write!(f, "{Pop}{Astar}"),
            PathFirst => write!(f, "{First}{Path}"),
            PathSignLen => write!(f, "{Sign}{Len}{Path}"),
            PathTake => write!(f, "{Take}…{Path}"),
            PathPop => write!(f, "{Pop}{Path}"),
            SplitByScalar => write!(f, "{Partition}{Box}{By}{Ne}"),
            SplitBy => write!(f, "{Partition}{Box}{Not}{By}{Mask}"),
            SplitByKeepEmpty => write!(f, "{Un}{Reduce}$\"_…_\""),
            AbsComplex => write!(f, "{Abs}{Complex}"),
            MatrixDiv => write!(f, "{Anti}{Under}{Transpose}({Reduce}{Add}{Mul})"),
            RangeStart => write!(f, "{Add}{Dip}{Range}"),
            &ReduceDepth(n) => {
                for _ in 0..n {
                    write!(f, "{Rows}")?;
                }
                write!(f, "{Reduce}(…)")?;
                Ok(())
            }
            &TransposeN(n) => {
                if n < 0 {
                    write!(f, "{Un}")?;
                    if n < -1 {
                        write!(f, "(")?;
                    }
                }
                for _ in 0..n.unsigned_abs() {
                    write!(f, "{Transpose}")?;
                }
                if n < -1 {
                    write!(f, ")")?;
                }
                Ok(())
            }
            &StackN { n, inverse } => {
                if inverse {
                    write!(f, "{Un}")?;
                }
                let n_str: String = (n.to_string().chars())
                    .map(|c| SUBSCRIPT_DIGITS[(c as u32 as u8 - b'0') as usize])
                    .collect();
                write!(f, "{Args}{n_str}")
            }
            SidedFill(side) => write!(f, "{Fill}{side}"),
            RepeatWithInverse => write!(f, "{Repeat}"),
            RepeatCountConvergence => write!(f, "{Un}{Repeat}"),
            ValidateType => write!(f, "{Un}…{Type}{Dup}"),
            ValidateTypeConsume => write!(f, "{Un}…{Type}"),
            TestAssert => write!(f, "{Assert}"),
            ValidateNonBoxedVariant => write!(f, "|…[…]"),
            ValidateVariant => write!(f, "|…°[…]"),
            TagVariant => write!(f, "<tag variant>"),
            BothImpl(sub) => write!(f, "{Both}{sub}"),
            UnBothImpl(sub) => write!(f, "{Un}{Both}{sub}"),
            Retropose => write!(f, "<{Evert}>"),
            FixMatchRanks => write!(f, "{Evert}"),
            Ga(op, _) => op.fmt(f),
            Over => write!(f, "over"),
            &MultiKeep(n) => {
                write!(f, "{Keep}")?;
                fmt_subscript(f, n as i32)
            }
            &SidedJoin(side) => write!(f, "{Join}{side}"),
            &DipN(n) => {
                for _ in 0..n {
                    write!(f, "{Dip}")?;
                }
                Ok(())
            }
            LayoutArgs => write!(f, "layout!"),
            VoxelsArgs => write!(f, "voxels!"),
        }
    }
}
