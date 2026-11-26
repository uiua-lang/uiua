use super::{ElemAlign, Grid, GridFmt, Metagrid};
use crate::{ArrayCmp, Primitive, Shape};
use smallvec::SmallVec;
use std::num::NonZero;

#[derive(Debug, Clone, PartialEq)]
pub enum Summary {
    All(String),
    Multiset(Vec<(String, usize)>),
    Boxes {
        ranks: (usize, usize),
        shapes: (Shape, Shape),
    },
    RangeMean {
        range: (String, String),
        mean: String,
    },
    RangeMeanWithNans {
        range: (String, String),
        mean: String,
        nan_count: NonZero<usize>,
    },
    Text(Vec<&'static str>),
    #[cfg(feature = "ga")]
    Multivectors {
        dims: Vec<u8>,
        grades: Vec<usize>,
        flavors: Vec<crate::GaFlavor>,
        range: (String, String),
    },
}

impl Summary {
    pub fn range_mean<T: GridFmt, U: GridFmt>(range: (T, T), mean: U) -> Self {
        let range = (range.0.grid_string(false), range.1.grid_string(false));
        let mean = mean.grid_string(false);
        Self::RangeMean { range, mean }
    }

    pub fn range_mean_with_nans(range: (f64, f64), mean: f64, nan_count: usize) -> Self {
        let range = (range.0.grid_string(false), range.1.grid_string(false));
        let mean = mean.grid_string(false);
        if let Some(nan_count) = NonZero::new(nan_count) {
            Self::RangeMeanWithNans {
                range,
                mean,
                nan_count,
            }
        } else {
            Self::RangeMean { range, mean }
        }
    }

    pub fn multiset<T: ArrayCmp + GridFmt + Clone>(elems: &[T]) -> Option<Self> {
        if elems.is_empty() {
            return None;
        }

        // Typically, one would use a BTreeMap here.
        // However, this function bails at a size limit that is expected to be very small.
        // So small, in fact, that a BTreeMap would only have one node anyway. (B = 6 -> node capacity = 11)
        // In addition, the usage of non-Ord keys (f64, Complex) would require a newtype.
        // This is plenty doable, but gets more annoying in a generic context.
        let mut pairs = SmallVec::<[(T, usize);
            5 // can be replaced with T::SUMMARY_MULTISET_MAX_DISTINCT on nightly compiler
              // but this is just a lower bound so it's ok
        ]>::new();
        for elem in elems.iter().cloned() {
            match pairs.iter().position(|p| T::array_eq(&p.0, &elem)) {
                Some(i) => pairs[i].1 += 1,
                None if pairs.len() >= T::SUMMARY_MULTISET_MAX_DISTINCT => return None,
                None => pairs.push((elem, 1)),
            }
        }
        pairs.sort_unstable_by(|a, b| T::array_cmp(&a.0, &b.0));

        if let [(val, _)] = &pairs[..] {
            return Some(Self::All(val.grid_string(false)));
        }

        let pairs = pairs
            .into_iter()
            .map(|(val, count)| (val.grid_string(false), count))
            .collect();

        Some(Self::Multiset(pairs))
    }

    pub fn to_grid(&self, multiline: bool) -> Grid {
        fn join<T: ToString>(delim: &str, iter: impl IntoIterator<Item = T>) -> String {
            iter.into_iter()
                .map(|v| v.to_string())
                .reduce(|a, b| a + delim + &b)
                .unwrap_or_default()
        }

        fn join_prefix<T: ToString>(
            prefix: impl ToString,
            delim: &str,
            iter: impl IntoIterator<Item = T>,
        ) -> String {
            iter.into_iter()
                .map(|v| v.to_string())
                .fold(prefix.to_string(), |a, b| a + delim + &b)
        }

        match self {
            Self::All(value) => Grid::from_utf8(&format!("all {value}")),
            Self::Multiset(pairs) => {
                let [dedup, occ] = [Primitive::Deduplicate, Primitive::Occurrences]
                    .map(|p| p.glyph().unwrap())
                    .map(Grid::from_scalar);

                let mut value_summary = pairs
                    .iter()
                    .map(|p| Grid::from_utf8(&p.0))
                    .collect::<Vec<_>>();
                let mut count_summary = pairs
                    .iter()
                    .map(|p| Grid::from_utf8(&p.1.to_string()))
                    .collect::<Vec<_>>();

                value_summary.insert(0, dedup);
                count_summary.insert(0, occ);

                let [a, b] = [value_summary, count_summary].map(Metagrid::from_row);
                let mut metagrid = if multiline { a.join_0(b) } else { a.join_1(b) };

                assert!(metagrid.is_rectangular());
                for y in 0..metagrid.len_0() {
                    metagrid.align_row(y);
                }
                for x in 0..metagrid.len_1() {
                    metagrid.align_column(x, ElemAlign::Center);
                }
                assert!(metagrid.is_meta_rectangular());

                for row in metagrid.rows_mut() {
                    for x in (1..row.len()).rev() {
                        row.insert(x, Grid::from_scalar(' '));
                    }
                }

                metagrid.flatten()
            }
            Self::Boxes {
                ranks: (min_rank, max_rank),
                shapes: (min_shape, max_shape),
            } => {
                let rows = if min_shape == max_shape {
                    vec![format!("all shape {min_shape}")] // uniform shape implies uniform rank
                } else if min_rank == max_rank {
                    vec![
                        format!("all rank {min_rank}"),
                        format!("shapes {min_shape}-{max_shape}"),
                    ]
                } else {
                    vec![
                        format!("ranks {min_rank}-{max_rank}"),
                        format!("shapes {min_shape}-{max_shape}"),
                    ]
                };

                if multiline {
                    let mut grid = Grid::from_utf8_rows(rows);
                    grid.align_center(grid.len_1());
                    grid
                } else {
                    Grid::from_utf8(&rows.join(", "))
                }
            }
            Self::RangeMean { range, mean } => {
                Grid::from_utf8(&format!("{}-{} μ{}", range.0, range.1, mean))
            }
            Self::RangeMeanWithNans {
                range,
                mean,
                nan_count,
            } => {
                let stats = format_args!("{}-{} μ{}", range.0, range.1, mean);
                let s = if nan_count.get() == 1 {
                    format!("{stats} (1 nan)")
                } else {
                    format!("{stats} ({nan_count} nans)")
                };
                Grid::from_utf8(&s)
            }
            Self::Text(parts) => {
                if multiline {
                    let mut grid = Grid::from_utf8_rows(parts);
                    grid.align_left(grid.len_1());
                    grid
                } else {
                    let s = match parts[..] {
                        [] => String::new(),
                        [part] => part.to_owned(),
                        [a, b] => format!("{a} and {b}"),
                        _ => parts.join(", "),
                    };
                    Grid::from_utf8(&s)
                }
            }
            #[cfg(feature = "ga")]
            Self::Multivectors {
                dims,
                grades,
                flavors,
                range: (min_mag, max_mag),
            } => {
                fn as_range<T: Copy + Into<usize>>(ns: &[T]) -> Option<(usize, usize)> {
                    // Multivector::summarize is expected to sort+dedup
                    assert!(ns.windows(2).all(|w| w[0].into() < w[1].into()));

                    let [first, last] = [ns.first()?, ns.last()?].map(|&n| n.into());
                    let is_range = last - first == ns.len();
                    is_range.then_some((first, last))
                }

                let dim_sum = match dims[..] {
                    [] => String::new(),
                    [dim] => format!("{dim}D"),
                    _ => match as_range(dims) {
                        Some((min, max)) => format!("{min}-{max}D"),
                        None => join(" ", dims.iter().map(|d| format!("{d}D"))),
                    },
                };
                let flavor_sum = match flavors[..] {
                    [] => String::new(),
                    [fl] => fl.to_string(),
                    _ => join("/", flavors),
                };
                let grade_sum = match grades[..] {
                    [] => "no grades".to_owned(),
                    [gr] => format!("grade {gr}"),
                    _ => match as_range(grades) {
                        Some((min, max)) => format!("grades {min}-{max}"),
                        None => join_prefix("grades", " ", grades),
                    },
                };

                let rows = [
                    dim_sum,
                    flavor_sum,
                    format!("({grade_sum})"),
                    format!("⌵{min_mag}-{max_mag}"),
                ];

                let mut grid = Grid::from_utf8_rows(rows.into_iter().filter(|s| !s.is_empty()));
                grid.align_center(grid.len_1());
                grid
            }
        }
    }
}
