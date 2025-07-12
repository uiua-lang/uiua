use leptos::*;
use leptos_router::*;
use uiua::{PrimClass, PrimDoc, PrimDocFragment, PrimDocLine, Primitive, SysOp};
use uiua_editor::Editor;

use crate::{Hd, Prim, Prims};

pub fn doc_line_fragments_to_view(fragments: &[PrimDocFragment]) -> View {
    if fragments.is_empty() {
        return view!( <br/>).into_view();
    }
    fragments
        .iter()
        .map(|frag| match frag {
            PrimDocFragment::Text(s) => s.into_view(),
            PrimDocFragment::Code(s) => view!(<code>{s}</code>).into_view(),
            PrimDocFragment::Emphasis(s) => view!(<em>{s}</em>).into_view(),
            PrimDocFragment::Strong(s) => view!(<strong>{s}</strong>).into_view(),
            PrimDocFragment::Link { text, url } => {
                let url = url.clone();
                let text = text.clone();
                view!(<a href=url>{text}</a>).into_view()
            }
            &PrimDocFragment::Primitive { prim, named } => {
                view!(<Prim prim=prim glyph_only={!named}/>).into_view()
            }
        })
        .collect::<Vec<_>>()
        .into_view()
}

fn doc_lines_to_view(lines: &[PrimDocLine]) -> impl IntoView {
    lines
        .iter()
        .map(|line| match line {
            PrimDocLine::Text(frags) => {
                view!(<p style="white-space: pre-wrap">{doc_line_fragments_to_view( frags)}</p>)
                    .into_view()
            }
            PrimDocLine::Example(ex) => view!(<Editor example={ ex.input() }/>).into_view(),
        })
        .collect::<Vec<_>>()
}

#[component]
pub fn PrimDocs(prim: Primitive) -> impl IntoView {
    let mut sig = String::new();
    if prim.class() == PrimClass::Constant {
        sig.push_str("Constant");
    } else if let Some(margs) = prim.modifier_args() {
        match margs {
            1 => sig.push_str("Monadic"),
            2 => sig.push_str("Dyadic"),
            3 => sig.push_str("Triadic"),
            n => sig.push_str(&format!("{n}-function")),
        }
        if let Some(args) = prim.args() {
            sig.push(' ');
            sig.push_str(&args.to_string());
            sig.push_str("-argument");
        }
        sig.push_str(" modifier");
    } else {
        match prim.args() {
            Some(0) => sig.push_str("Noadic"),
            Some(1) => sig.push_str("Monadic"),
            Some(2) => sig.push_str("Dyadic"),
            Some(3) => sig.push_str("Triadic"),
            Some(n) => sig.push_str(&format!("{n}-argument")),
            None => sig.push_str("Variadic"),
        }
        if let Some(outputs) = prim.outputs() {
            if outputs != 1 {
                sig.push_str(&format!(" {outputs}-output"));
            }
        } else {
            sig.push_str(" variable-output");
        }
        if prim.class().is_pervasive() {
            sig.push_str(" pervasive");
        }
        sig.push_str(" function");
    }
    let long_name = if let Primitive::Sys(op) = prim {
        Some(format!(" - {}", op.long_name()))
    } else {
        None
    };
    let doc = PrimDoc::from(prim);
    let body = view! {
        <p style="white-space: pre-wrap">{doc_line_fragments_to_view(&doc.short)}</p>
        { doc_lines_to_view(&doc.lines) }
    };

    let id = prim.name();

    let experimental = prim.is_experimental().then(|| {
        view! {
            <p><span class="experimental">"⚠️ Warning 🧪: This "{if prim.is_modifier() { "modifier" }else{ "function" }}" is experimental and may be changed or removed in the future."</span><span class="output-fainter">" Experimental features can be enabled by putting an "<code>"# Experimental!"</code>" comment at the top of a program."</span></p>
        }
    });

    let aliases: Vec<_> = prim
        .aliases()
        .iter()
        .copied()
        .filter(|&s| s != prim.name())
        .collect();
    let aliases = if aliases.is_empty() {
        None
    } else {
        Some(view!(<p style="white-space: pre-wrap">
                "Aliases: "{aliases.iter().map(|&s| view!(<code>{s}</code>" ")).collect::<Vec<_>>()}
            </p>))
    };

    view! {
        <div>
            <h1 id=id><Prim prim=prim hide_docs=true/>{ long_name }</h1>
            <p><h3>{ sig }</h3></p>
            { aliases }
            <p>{ experimental }</p>
            { body }
            { match prim {
                Primitive::Un => all_uns().into_view(),
                Primitive::Under => all_unders().into_view(),
                Primitive::Fill => all_fills().into_view(),
                _ => View::default(),
            } }
        </div>
    }
}

fn all_fills() -> impl IntoView {
    use Primitive::*;
    view! {
        <Hd id="fills"><Prim prim=Fill/>"-compatible functions"</Hd>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th>"Function"</th>
                <th>"Notes"</th>
                <th>"Example"</th>
            </tr>
            { fill_row_impl("Pervasive Dyadics", "", "⬚10+ [1 2] [3 4 5 6]") }
            { fill_row_impl("Arrays", "", "⬚0[1 2_3_4 5_6]") }
            { fill_row(First, "Default scalar for empty array", "⬚5⊢ []") }
            { fill_row(Parse, "Default for non-number strings", "⬚10⋕ {\"1\" \"2\" \"dog\"}") }
            { fill_row(Abs, "Multi-character uppercase", r#"⬚@-⌵ ["a" "ß"]"#) }
            { fill_row(Couple, "Matches shapes", "⬚0⊟ [1 2 3 4] [5 6]") }
            { fill_row(Join, "Makes shapes work", "⬚0⊂ [1_2 3_4] [5 6 7]") }
            { fill_row(Keep, "Fills mask", "⬚0▽ [1 0 1] \"abcdef\"") }
            { fill_row(Pick, "Out-of-bounds default", "⬚10⊡ 5 [1 2 3]") }
            { fill_row(Select, "Out-of-bounds default", "⬚10⊏ 5 [1 2 3]") }
            { fill_row(Take, "Out-of-bounds default", "⬚0↙5 [1 2 3]") }
            { fill_row(Reshape, "Fills excess elements", "⬚0↯ 2_4 [1 2 3]") }
            { fill_row(Rotate, "Fills instead of wrapping", "⬚0↻ 2 [1 2 3 4 5]") }
            { fill_row(Reduce, "Sets initial value", "⬚10/+ [1 2 3]") }
            { fill_row(Scan, "Sets initial value and fills row shapes", "⬚10\\⊂ [1 2 3]") }
            { fill_row(Rows, "Fills row shapes", "⬚0≡⇡ [4 7 3]") }
            { fill_row(Partition, "Fills row shapes", "⬚@ ⊜∘ ≠@ . \"Hey there\"") }
            { fill_row(Group, "Fills row shapes", "⬚0⊕∘ ◿3. [1 8 4 9 3 8 2]") }
            { fill_row(Base, "Repeating base", "⬚10base[12 20] 999999") }
            { fill_row_impl(view!(<Prims prims=[Un, Pop]/>), "Get fill value", "⬚5°◌") }
        </table>
    }
}

fn fill_row(
    prim: Primitive,
    notes: impl IntoView,
    example: impl Into<Option<&'static str>>,
) -> impl IntoView {
    fill_row_impl(view!(<Prim prim=prim/>), notes, example)
}

fn fill_row_impl(
    prim: impl IntoView,
    notes: impl IntoView,
    example: impl Into<Option<&'static str>>,
) -> impl IntoView {
    view! {
        <tr>
            <td>{prim}</td>
            <td>{notes}</td>
            <td>{ example.into().map(|ex| view!(<Editor example=ex/>)) }</td>
        </tr>
    }
}

fn all_uns() -> impl IntoView {
    use Primitive::*;
    use SysOp::*;
    use ValueRequirement::*;
    view! {
        <Hd id="uns"><Prim prim=Un/>"-compatible functions"</Hd>
        <p>"These functions are also compatible with "<Prim prim=Under/>"."</p>
        <p>"See the "<A href="/docs/under#unders">"similar table"</A>" for "<Prim prim=Under/>" for more."</p>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th>"Pattern"</th>
                <th>"Value?"<sup>" "<span
                    style="text-decoration: underline dotted; font-size: 0.8em; cursor: help;"
                    title="Whether a constant value can be inside the inverted function">
                    "*"
                </span></sup></th>
                <th>"Notes"</th>
                <th>"Example"</th>
            </tr>
            { inverse_row([Un], No, "", "°°⊟ 1 2") }
            { inverse_row_impl("Constant", Optional, "Pattern match", "°8 8") }
            { inverse_row([Dup], No, "Pattern match", "°. 4 4") }
            { inverse_row([Min], Required, "Pattern match", "°(↧5) 3") }
            { inverse_row([Max], Required, "Pattern match", "°(↥3) 5") }
            { inverse_row([Identity], No, "Self inverse", "°∘ 5") }
            { inverse_row([Neg], No, "Self inverse", "°¯ 5") }
            { inverse_row([Not], No, "Self inverse", "°¬ 5") }
            { inverse_row([Reciprocal], No, "Self inverse", "°⨪ 5") }
            { inverse_row([Sqrt], No, "", "°√ 5") }
            { inverse_row([Sin], No, "Arcsine", "°∿ 1") }
            { inverse_row([Add], Required, "", "°(+1) 5") }
            { inverse_row([Sub], Required, "", "°(-1) 5") }
            { inverse_row([Mul], Required, "", "°(×2) 5") }
            { inverse_row([Div], Required, "", "°(÷2) 5") }
            { inverse_row([Pow], Required, "", "°(ⁿ2) 36") }
            { inverse_row([Exp], No, "Natural logarithm", "°ₑ 10") }
            { inverse_row([Add, Dup], No, "", "°(+.) 10") }
            { inverse_row([Mul, Dup], No, "", "°(×.) 16") }
            { inverse_row([Atan], No, view!(<Prim prim=Sin/>" and cosine"), "°∠ π") }
            { inverse_row([Complex], No, "Complex part on top", "°ℂ i") }
            { inverse_row([Couple], No, view!("Array must have "<Prim prim=Len/>" 2"), "°⊟ [1_2_3 4_5_6]") }
            { inverse_row_impl(
                view!(<code>"["</code><Prims prims=[Dip, Dip]/><code>"…"</code><Prims prims=[Identity]/><code>"]"</code>),
                No,
                view!("Array must have compatible "<Prim prim=Len/>),
                "°[⊙⊙∘] [1 2 3]"
            ) }
            { inverse_row_impl(
                view!(<code>"{"</code><Prims prims=[Dip, Dip]/><code>"…"</code><Prims prims=[Identity]/><code>"}"</code>),
                No,
                view!("Array must have compatible "<Prim prim=Len/>),
                "°{⊙⊙∘} {1 2_3 4}"
            ) }
            { inverse_row_impl(view!(<code>"[…]"</code>), No, "", "°[⇌] [[1 2 3]]") }
            { inverse_row_impl(view!(<code>"{…}"</code>), No, "", "°{⊙∘} {1 2_3}") }
            { inverse_row([Box], No, "No-op on non-scalars and non-boxes", "°□ □[1 2 3]") }
            { inverse_row([Reverse], No, "Self inverse", "°⇌ [1 2 3 4]") }
            { inverse_row([Shape], No, "", "°△ [2 2 4]") }
            { inverse_row([Transpose], No, "", "°⍉ ↯2_3_2⇡12") }
            { inverse_row([Bits], No, "", "°⋯ [1 0 1]") }
            { inverse_row([Where], No, "", "°⊚ [1 4]") }
            { inverse_row([Parse], No, "", "°⋕ [8 9 10 11 12]") }
            { inverse_row([Fix], No, "", "°¤ [[1 2 3]]") }
            { inverse_row([Sort], No, "Shuffle", "°⍆ [1 2 3 4]") }
            { inverse_row([Utf8], No, "", "°utf [240 159 145 139 32 72 105 33]") }
            { inverse_row([Csv], No, "", "°csv \"1,2\\n3,4\"") }
            { inverse_row([Rotate], Required, "", "°(↻1) [1 2 3 4]") }
            { inverse_row([Join], No, "", "°⊂ [1 2 3 4]") }
            { inverse_row([Join], Required, "Pattern match", "°(⊂1_2) [1 2 3 4]") }
            { inverse_row([Keep], No, "", "°▽ [1 1 1 2 1 1 3 3]") }
            { inverse_row([Select], No, "", "°⊏ \"hello\"") }
            { inverse_row([Pick], No, "", "°⊡ [1_2_3 4_5_6]") }
            { inverse_row([Orient], No, "", "°⤸ [1_2_3 4_5_6]") }
            { inverse_row([Scan], No, view!("Only works with "<Prims prims=[Add, Mul, Eq, Ne]/>), "°\\+ [1 3 6 10 15]") }
            { inverse_row([Reduce, Mul], No, "Prime factors", "°/× 60") }
            { inverse_row_impl(
                view!(<Prim prim=Reduce/><code class="dyadic-function">"dyadic"</code>),
                No,
                "Inner function must be invertible",
                "°/ℂ i"
            ) }
            { inverse_row_impl(view!(<code class="string-literal-span">"$\"_…_\""</code>), No, "Pattern match and extract", "{°$\"_ - _(_)_\"} \"a - bc(de) - f\"") }
            { inverse_row_impl(view!(<Prims prims=[Reduce]/><code class="string-literal-span">"$\"_…_\""</code>), No, "Must have 2 args and only text between", "°/$\"_ - _\" \"a - bc - d\"") }
            { inverse_row([Partition], No, "Inner function must be invertible", "°⊜□ {\"Hey\" \"there\" \"buddy\"}") }
            { inverse_row([Group], No, "Inner function must be invertible", "°⊕□ {1 2_3_4 5_6}") }
            { inverse_row([Repeat], Required, "Inner function must be invertible", "°(⍥(×2)5) 1024") }
            { inverse_row([Stack], No, "", "°? 5") }
            { inverse_row([Dump], No, "", "°dump△ [2 3 4]") }
            { inverse_row([Pop], RequiresFill, "", "⬚5°◌") }
            { inverse_row([AudioEncode], Optional, "Decodes bytes", None) }
            { inverse_row([ImageEncode], Optional, "Decodes bytes", None) }
            { inverse_row([GifEncode], Optional, "Decodes bytes", None) }
            { inverse_row([Sys(Clip)], No, "Set the clipboard", None) }
            { inverse_row([Sys(RawMode)], No, "Terminal raw state", None) }
        </table>
    }
}

fn all_unders() -> impl IntoView {
    use Primitive::*;
    use SysOp::*;
    use ValueRequirement::*;
    view! {
        <Hd id="unders"><Prim prim=Under/>"-compatible functions"</Hd>
        <p>"Any function that is compatible with "<Prim prim=Un/>" is also compatible with "<Prim prim=Under/>"."</p>
        <p>"Functions that are compatible with "<Prim prim=Under/>" that are either not compatible with "<Prim prim=Un/>" or have different behavior are listed below."</p>
        <p>"See the "<A href="/docs/un#uns">"similar table"</A>" for "<Prim prim=Un/>" for more."</p>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th style="width: 20%">"Pattern"</th>
                <th>"Value?"<sup>" "<span
                    style="text-decoration: underline dotted; font-size: 0.8em; cursor: help;"
                    title="Whether a constant value can be inside the inverted function">
                    "*"
                </span></sup></th>
                <th style="width: 18%">"Notes"</th>
                <th>"Example"</th>
            </tr>
            { inverse_row([Un], Optional, "Inner function must be invertible", "⍜°⊟(+5) [1 2]") }
            { inverse_row([Add], Optional, view!("Optional "<Prim prim=Backward/>), "⍜+(×2) 1 5") }
            { inverse_row([Sub], Optional, view!("Optional "<Prim prim=Backward/>), "⍜-(×2) 1 5") }
            { inverse_row([Mul], Optional, view!("Optional "<Prim prim=Backward/>), "⍜×(+1) 2 5") }
            { inverse_row([Div], Optional, view!("Optional "<Prim prim=Backward/>), "⍜÷(+1) 2 5") }
            { inverse_row([Modulus], Optional, view!("Optional "<Prim prim=Backward/>), "⍜◿(×10) 4 9") }
            { inverse_row([Pow], Optional, view!("Optional "<Prim prim=Backward/>), "⍜ⁿ(-9) 2 5") }
            { inverse_row([Exp], No, "", "⍜ₑ(ⁿ2) 7") }
            { inverse_row([Get], Optional, "", "⍜get(×10) @b map \"abc\" 1_2_3") }
            { inverse_row([Floor], No, "", "⍜⌊(×10) 1.5") }
            { inverse_row([Ceil], No, "", "⍜⌈(×10) 1.5") }
            { inverse_row([Round], No, "", "⍜⁅(×10) 1.5") }
            { inverse_row([Abs], No, "", "⍜⌵(+1) ¯5") }
            { inverse_row([Sign], No, "", "⍜±(×2) ¯5") }
            { inverse_row([First], No, "", "⍜⊢(×10) [1 2 3 4 5]") }
            { inverse_row([Last], No, "", "⍜⊣(×10) [1 2 3 4 5]") }
            { inverse_row([Shape], No, "Tiles", "⍜△(×2) [1_2_3 4_5_6]") }
            { inverse_row([Len], No, view!(<Prim prim=Reshape/>), "⍜⧻(+1) [1_2_3 4_5_6]") }
            { inverse_row([Deshape], No, "", "⍜♭⇌ ↯3_3⇡9") }
            { inverse_row([Rise], No, "", "⍜⍏(↻¯1). [1 4 2 3 5]") }
            { inverse_row([Fall], No, "", "⍜⍖(↻¯1). [1 4 2 3 5]") }
            { inverse_row([Sort], No, "", "⍜⍆(↻¯1). [1 4 2 3 5]") }
            { inverse_row([Where], No, "Maintains minumum shape", "⍜⊚⊂ [1 0 0 0 0] 3") }
            { inverse_row([Classify], No, "", "⍜⊛⇌ \"hello\"") }
            { inverse_row([Deduplicate], No, "", "⍜◴⇌ \"hello\"") }
            { inverse_row([Deshape], No, "Works with subscripts", "⍜♭⇌ ↯2_2_4⇡16") }
            { inverse_row([Reshape], Optional, "", "⍜↯⇌ 2_3 ⇡6") }
            { inverse_row([Take], Optional, "", "⍜↙(×10) 2 [1 2 3 4 5]") }
            { inverse_row([Drop], Optional, "", "⍜↘(×10) 2 [1 2 3 4 5]") }
            { inverse_row([Keep], Optional, "", "⍜▽(×10) ⊸◿2 [1 2 3 4 5]") }
            { inverse_row([Rotate], Optional, "", "⍜(↻2|⊂π) [1 2 3 4 5]") }
            { inverse_row([Select], Optional, "Duplicate indices must have the same value", "⍜(⊏1_4|×10) [1 2 3 4 5]") }
            { inverse_row([Pick], Optional, "Duplicate indices must have the same value", "⍜(⊡1_1|×10) [1_2_3 4_5_6]") }
            { inverse_row([Join], No, view!(<Prim prim=Len/>" may not change"), "⍜⊂\\+ 1_2_3 4_5_6") }
            { inverse_row([Base], Optional, "", "⍜⊥(⊂1) 10 123") }
            { inverse_row([Stencil, Identity], Optional, "Chunking only", "⍜⧈∘≡⇌ ¤¤3 ⇡9") }
            { inverse_row([Dip], No, "Inner function must be invertible", "⍜⊙⊂× 10 2 3") }
            { inverse_row([Both], No, "Inner function must be invertible", "⍜∩⊡∩(×10) 1 [1 2 3] 2 [4 5 6]") }
            { inverse_row([Pop], No, "", "⍜◌(×2) 1 2") }
            { inverse_row([Rows], No, "Inner function must be invertible", "⍜≡⊢(×10) [1_2_3 4_5_6]") }
            { inverse_row([Inventory], No, "Inner function must be invertible", "⍜⍚△⇌ {1 2_3 4_5_6}") }
            { inverse_row([Group], No, "Inner function must be invertible", "⍜⊕□≡⇌ ⊸≠@ \"I love arrays\"") }
            { inverse_row([Partition], No, "Inner function must be invertible", "⍜⊜□≡⇌ ⊸≠@ \"Hello World\"") }
            { inverse_row([Fold], No, "Inner function must be invertible", "⍜∧⊏(×10) [0 2] ↯2_3⇡6") }
            { inverse_row([Repeat], Required, "Inner function must be invertible", "⍜⍥(×2). 5 1") }
            { inverse_row([Repeat], No, "Inner function must be invertible", "°⍥°(↧1000×2) 5") }
            { inverse_row([Switch], No, "", "⍜(⨬⊢⊣|×10) 1 [1 2 3 4]") }
            { inverse_row([Now], No, "Times execution", "⍜now(&sl 0.005)") }
            { inverse_row([Sys(FOpen)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(FCreate)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(RunStream)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(TcpConnect)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(TcpAccept)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(FReadAllStr)], Optional, view!("Calls "<Prim prim=Sys(FWriteAll)/>), None) }
            { inverse_row([Sys(FReadAllBytes)], Optional, view!("Calls "<Prim prim=Sys(FWriteAll)/>), None) }
            { inverse_row([Sys(RawMode)], Optional, "Resets raw state", None) }
        </table>
    }
}

enum ValueRequirement {
    No,
    Optional,
    Required,
    RequiresFill,
}

fn inverse_row<const N: usize>(
    prims: [Primitive; N],
    value_req: ValueRequirement,
    notes: impl IntoView,
    example: impl Into<Option<&'static str>>,
) -> impl IntoView {
    inverse_row_impl(
        view!(<h3><Prims prims=prims show_names={N == 1}/></h3>).into_view(),
        value_req,
        notes.into_view(),
        example.into(),
    )
}

fn inverse_row_impl(
    prims: impl IntoView,
    value_req: ValueRequirement,
    notes: impl IntoView,
    example: impl Into<Option<&'static str>>,
) -> impl IntoView {
    view! {
        <tr>
            <td>{prims}</td>
            <td>{ match value_req {
                ValueRequirement::No => "No".into_view(),
                ValueRequirement::Optional => "Optional".into_view(),
                ValueRequirement::Required => "Required".into_view(),
                ValueRequirement::RequiresFill => view!(<Prim prim=Primitive::Fill/>).into_view(),
            } }</td>
            <td>{notes}</td>
            <td>{ example.into().map(|ex| view!(<Editor example=ex/>)) }</td>
        </tr>
    }
}

#[cfg(test)]
#[test]
fn prim_docs() {
    use uiua::{PrimDocLine, Uiua};
    use uiua_editor::backend::WebBackend;

    for prim in Primitive::non_deprecated() {
        let doc = PrimDoc::from(prim);
        for line in &doc.lines {
            if let PrimDocLine::Example(ex) = line {
                if [
                    "&sl", "&tcpc", "&tlsc", "&ast", "&clip", "&frab", "&fmd", "&b",
                ]
                .iter()
                .any(|prim| ex.input().contains(prim))
                {
                    continue;
                }
                eprintln!("{prim} example:\n{}", ex.input());
                match Uiua::with_backend(WebBackend::default()).run_str(ex.input()) {
                    Ok(mut comp) => {
                        if let Some(diag) = comp.take_diagnostics().into_iter().next() {
                            if !ex.should_error() {
                                panic!("\nExample failed:\n{}\n{}", ex.input(), diag.report());
                            }
                        } else if ex.should_error() {
                            panic!("Example should have failed: {}", ex.input());
                        }
                    }
                    Err(e) => {
                        if !ex.should_error() {
                            panic!("\nExample failed:\n{}\n{}", ex.input(), e.report());
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
#[test]
fn listed_examples() {
    use uiua::Uiua;
    use uiua_editor::backend::WebBackend;

    let text = std::fs::read_to_string("src/primitive.rs").unwrap();
    for mut line in text.lines() {
        line = line.trim();
        if !line.starts_with("{ ") {
            continue;
        }
        if line.ends_with(", None) }") {
            continue;
        }
        let line = line.replace("\\\"", "<double quote>");
        let Some(end) = line.rfind("\"") else {
            continue;
        };
        eprintln!("{line}");
        let Some(start) = line[..end]
            .rfind("r#\"")
            .map(|i| i + 3)
            .or_else(|| line[..end].rfind('"').map(|i| i + 1))
        else {
            panic!("No start quote in line: {line}");
        };
        let example = line[start..end]
            .replace("<double quote>", "\"")
            .replace("\\\\", "\\");
        let mut env = Uiua::with_backend(WebBackend::default());
        if let Err(e) = env.run_str(&example) {
            panic!("Example failed: {example}\n{e}");
        }
    }
}
