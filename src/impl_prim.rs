//! All primitive definitions

use serde::*;

use crate::{Purity, SubSide, Subscript, algorithm::ga};

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
            UndoRowsSub(Subscript<i32>, bool),
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
                    ImplPrimitive::MultiJoin(n) => *n,
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
    (1, SortedUp),
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
    (1, SquareAbs),
    (2, MatrixDiv),
    (1, RangeSub(i32)),
    (1, Exp2),
    (1, Exp10),
    (1, Log2),
    (1, Log10),
    // Implementation details
    (2(3), Over),
    ([1], DipN(usize)),
    (1, NBits(usize)),
    (1, DeshapeSub(i32)),
    ([1], TableSub(i32)),
    (1, ClassifySub(usize)),
    (1, ParseSub(usize)),
    (1, TransposeN(i32)),
    (2, MultiKeep(usize)),
    (2, SidedJoin(SubSide)),
    ([1], SidedStencil(SubSide)),
    ((1), MultiJoin(usize)),
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
    ([1], FoldGif),
);
