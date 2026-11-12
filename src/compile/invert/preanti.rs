use super::*;

pub(super) fn pre_anti_inverse(input: &[Node], asm: &Assembly) -> InversionResult<(Node, Node)> {
    let mut pre = Node::empty();
    let mut post = Node::empty();
    let mut curr = input;
    let mut error = Generic;
    'find_pattern: loop {
        for pattern in PRE_ANTI_PATTERNS {
            match pattern.pre_anti_extract(curr, asm) {
                Ok((new, post_part, pre_part)) => {
                    dbgln!(
                        "matched pre-anti pattern {pattern:?}\n  \
                        on {curr:?}\n  to {post_part:?} and {pre_part:?}"
                    );
                    pre.push(pre_part);
                    post.prepend(post_part);
                    if new.is_empty() {
                        dbgln!(
                            "pre-anti-inverted\n  {input:?}\n  \
                            to {post:?} and {pre:?}"
                        );
                        return Ok((post, pre));
                    }
                    curr = new;
                    continue 'find_pattern;
                }
                Err(e) => error = error.max(e),
            }
        }
        break;
    }
    Err(error)
}

const PRE_ANTI_PATTERNS: &[&dyn PreAntiPattern] = &[
    &PreAntiMonadicPat,
    &PreAntiDipPat,
    // &PreAntiForkPat
];

trait PreAntiPattern: fmt::Debug + Sync {
    fn pre_anti_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node, Node)>;
}

preanti!(PreAntiMonadicPat, input, _, {
    for i in (input.len().min(1)..=input.len()).rev() {
        let sig = nodes_sig(&input[..i])?;
        if sig == (0, 0) || sig == (1, 1) {
            return Ok((&input[i..], Node::empty(), Node::from(&input[..i])));
        }
    }
    generic()
});

preanti!(PreAntiDipPat, input, asm, Dip, _span, [f], {
    let f_inv = f.un_inverse(asm)?.node;
    Ok((input, f_inv, Node::empty()))
});

// preanti!(PreAntiForkPat, input, asm, Fork, span, [f, g], {
//     let f = if f.sig == (1, 1) {
//         f.clone()
//     } else if f.sig == (2, 1) {
//         f.anti_inverse(asm)?
//     } else {
//         return generic();
//     };
//     let g = if g.sig == (1, 1) {
//         g.clone()
//     } else if g.sig == (2, 1) {
//         g.anti_inverse(asm)?
//     } else {
//         return generic();
//     };
//     let args = eco_vec![f, g];
//     Ok((input, Node::empty(), Mod(Fork, args, span)))
// });

macro_rules! preanti {
    // Optional parens
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), $body:expr) => {
        preanti!($(#[$attr])* $($doc,)? $($tt)*, $body);
    };
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), ref, $pat:pat, $body:expr) => {
        preanti!($(#[$attr])* $($doc,)? $($tt)*, ref, $pat, $body);
    };
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), $pat:pat, $body:expr) => {
        preanti!($(#[$attr])* $($doc,)? $($tt)*, $pat, $body);
    };
    // Main impl
    ($(#[$attr:meta])* $($doc:literal,)? $name:ident, $input:ident, $asm:tt, $body:expr) => {
        #[derive(Debug)]
        $(#[$attr])*
        $(#[doc = $doc])?
        struct $name;
        impl PreAntiPattern for $name {
            fn pre_anti_extract<'a>(
                &self,
                $input: &'a [Node],
                $asm: &Assembly,
            ) -> InversionResult<(&'a [Node], Node, Node)> {
                $body
            }
        }
    };
    // Ref pattern
    ($(#[$attr:meta])* $($doc:literal)? $name:ident, $input:ident, $asm:tt, ref, $pat:pat, $body:expr) => {
        preanti!($([$attr])* $($doc)? $name, $input, $asm, {
            let [$pat, $input @ ..] = $input else {
                return generic();
            };
            $body
        });
    };
    // Non-ref pattern
    ($(#[$attr:meta])* $($doc:literal)? $name:ident, $input:ident, $asm:tt, $pat:pat, $body:expr) => {
        preanti!($([$attr])* $($doc)? $name, $input, $asm, {
            let &[$pat, ref $input @ ..] = $input else {
                return generic();
            };
            $body
        });
    };
    // Mod pattern
    ($(#[$attr:meta])* $($doc:literal)? $name:ident, $input:ident, $asm:tt, $prim:ident, $span:ident, $args:pat, $body:expr) => {
        preanti!($([$attr])* $($doc)? $name, $input, $asm, ref, Mod($prim, args, $span), {
            #[allow(irrefutable_let_patterns)]
            let $args = args.as_slice() else {
                return generic();
            };
            let $span = *$span;
            $body
        });
    };
}
use preanti;
