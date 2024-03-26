use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use uiua::{PrimClass, PrimDocFragment, PrimDocLine, Primitive, SysOp};

use crate::{editor::Editor, Hd, Prim, Prims};

fn doc_line_fragments_to_view(fragments: &[PrimDocFragment]) -> View {
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
    let doc = prim.doc();
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

    view! {
        <div>
            <h1 id=id><Prim prim=prim hide_docs=true/>{ long_name }</h1>
            <p><h3>{ sig }</h3></p>
            <p>{ experimental }</p>
            { body }
            { match prim {
                Primitive::Un => all_uns().into_view(),
                Primitive::Under => all_unders().into_view(),
                _ => View::default(),
            } }
        </div>
    }
}

#[component]
pub fn AllFunctions() -> impl IntoView {
    let (list, set_list) =
        create_signal(view!(<h3 class="running-text">"Generating list..."</h3>).into_view());
    set_timeout(
        move || {
            set_list.set(
                Primitive::non_deprecated()
                    .map(|p| {
                        view! {
                            <br/>
                            <hr/>
                            <PrimDocs prim=p/>
                        }
                    })
                    .collect::<Vec<_>>()
                    .into_view(),
            )
        },
        Default::default(),
    );
    view! {
        <Title text="All Functions - Uiua Docs"/>
        <h1>"All Functions"</h1>
        <p>"This is a list of every built-in function in Uiua, provided for your scrolling pleasure."</p>
        <p>"For a searchable list, see the "<A href="/docs#functions">"main docs page"</A>"."</p>
        { move || list.get() }
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
            { inverse_row([Identity], No, "Self inverse", "°∘ 5") }
            { inverse_row([Flip], No, "Self inverse", "°: 2 5") }
            { inverse_row([Neg], No, "Self inverse", "°¯ 5") }
            { inverse_row([Not], No, "Self inverse", "°¬ 5") }
            { inverse_row([Sqrt], No, "", "°√ 5") }
            { inverse_row([Sin], No, "Arcsine", "°∿ 1") }
            { inverse_row([Add], Required, "", "°(+1) 5") }
            { inverse_row([Sub], Required, "", "°(-1) 5") }
            { inverse_row([Mul], Required, "", "°(×2) 5") }
            { inverse_row([Div], Required, "", "°(÷2) 5") }
            { inverse_row([Pow], Required, "", "°(ⁿ2) 36") }
            { inverse_row([Log], Required, "", "°(ₙ2) 8") }
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
            { inverse_row_impl(view!(<code>"{…}"</code>), No, "", "°{:} {1 2_3}") }
            { inverse_row([Box], No, "No-op on non-scalars and non-boxes", "°□ □[1 2 3]") }
            { inverse_row([Reverse], No, "", "°⇌ [1 2 3 4]") }
            { inverse_row([Transpose], No, "", "°⍉ ↯2_3_2⇡12") }
            { inverse_row([Bits], No, "", "°⋯ [1 0 1]") }
            { inverse_row([Where], No, "", "°⊚ [1 4]") }
            { inverse_row([Parse], No, "", "°⋕ [8 9 10 11 12]") }
            { inverse_row([Fix], No, "", "°¤ [[1 2 3]]") }
            { inverse_row([Utf], No, "", "°utf [240 159 145 139 32 72 105 33]") }
            { inverse_row([Csv], No, "", "°csv \"1,2\\n3,4\"") }
            { inverse_row([Rotate], Required, "", "°(↻1) [1 2 3 4]") }
            { inverse_row([Join], No, "", "°⊂ [1 2 3 4]") }
            { inverse_row([Join], Required, "Pattern match", "°(⊂1) [1 2 3 4]") }
            { inverse_row([Scan], No, view!("Only works with "<Prims prims=[Add, Mul, Eq, Ne]/>), "°\\+ [1 3 6 10 15]") }
            { inverse_row([Reduce, Mul], No, "Prime factors", "°/× 60") }
            { inverse_row([Repeat], No, "Inner function must be invertible", "°⍥(×2)5 1024") }
            { inverse_row([Trace], No, "", "°⸮ 5") }
            { inverse_row([Stack], No, "", "°? 5") }
            { inverse_row([Dump], No, "", "°dump△ [2 3 4]") }
            { inverse_row([Pop], RequiresFill, "", "⬚5°◌") }
            { inverse_row([Sys(AudioEncode)], No, "Decodes bytes", None) }
            { inverse_row([Sys(ImEncode)], No, "Decodes bytes", None) }
            { inverse_row([Sys(GifEncode)], No, "Decodes bytes", None) }
            { inverse_row([Sys(ClipboardGet)], No, "", None) }
            { inverse_row([Sys(ClipboardSet)], No, "", None) }
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
            { inverse_row([Un], Optional, "Inner function must be invertible", "⍜°⊟: [1 2]") }
            { inverse_row([Add], Optional, view!("Optional "<Prim prim=Flip/>), "⍜+(×2) 1 5") }
            { inverse_row([Sub], Optional, view!("Optional "<Prim prim=Flip/>), "⍜-(×2) 1 5") }
            { inverse_row([Mul], Optional, view!("Optional "<Prim prim=Flip/>), "⍜×(+1) 2 5") }
            { inverse_row([Div], Optional, view!("Optional "<Prim prim=Flip/>), "⍜÷(+1) 2 5") }
            { inverse_row([Mod], Optional, view!("Optional "<Prim prim=Flip/>), "⍜◿(×10) 4 9") }
            { inverse_row([Pow], Optional, view!("Optional "<Prim prim=Flip/>), "⍜ⁿ(-9) 2 5") }
            { inverse_row([Log], Optional, view!("Optional "<Prim prim=Flip/>), "⍜ₙ(+1) 3 8") }
            { inverse_row([Floor], No, "", "⍜⌊(×10) 1.5") }
            { inverse_row([Ceil], No, "", "⍜⌈(×10) 1.5") }
            { inverse_row([Round], No, "", "⍜⁅(×10) 1.5") }
            { inverse_row([Abs], No, "", "⍜⌵(+1) ¯5") }
            { inverse_row([Sign], No, "", "⍜±(×2) ¯5") }
            { inverse_row([First], No, "", "⍜⊢(×10) [1 2 3 4 5]") }
            { inverse_row([First, Reverse], No, "", "⍜(⊢⇌|×10) [1 2 3 4 5]") }
            { inverse_row([Deshape], No, "", "⍜♭⇌ ↯3_3⇡9") }
            { inverse_row([Rise], No, "", "⍜⍏(↻¯1). [1 4 2 3 5]") }
            { inverse_row([Fall], No, "", "⍜⍖(↻¯1). [1 4 2 3 5]") }
            { inverse_row([Classify], No, "", "⍜⊛⇌ \"hello\"") }
            { inverse_row([Deduplicate], No, "", "⍜◴⇌ \"hello\"") }
            { inverse_row([Rerank], Optional, "", "⍜(☇1)⇌ ↯2_2_4⇡16") }
            { inverse_row([Reshape], Optional, "", "⍜↯⇌ 2_3 ⇡6") }
            { inverse_row([Take], Optional, "", "⍜↙(×10) 2 [1 2 3 4 5]") }
            { inverse_row([Drop], Optional, "", "⍜↘(×10) 2 [1 2 3 4 5]") }
            { inverse_row([Keep], Optional, "", "⍜▽(×10) ◿2. [1 2 3 4 5]") }
            { inverse_row([Rotate], Optional, "", "⍜(↻2|⊂π) [1 2 3 4 5]") }
            { inverse_row([Pick], Optional, "Duplicate indices must have the same value", "⍜(⊡1_1|×10) [1_2_3 4_5_6]") }
            { inverse_row([Select], Optional, "Duplicate indices must have the same value", "⍜(⊏1_4|×10) [1 2 3 4 5]") }
            { inverse_row([Join], No, view!(<Prim prim=Len/>" may not change"), "⍜⊂\\+ 1_2_3 4_5_6") }
            { inverse_row([Dip], No, "Inner function must be invertible", "⍜⊙⊂× 10 2 3") }
            { inverse_row([Both], No, "Inner function must be invertible", "⍜∩⊡: 1 [1 2 3] 2 [4 5 6]") }
            { inverse_row([Pop], No, "", "⍜◌(×2) 1 2") }
            { inverse_row([Rows], No, "Inner function must be invertible", "⍜≡⊢(×10) [1_2_3 4_5_6]") }
            { inverse_row([Each], No, "Inner function must be invertible", "⍜∵⇌⍚\\+ {1_2_3 4_5}") }
            { inverse_row([Group], No, "Inner function must be invertible", "⍜⊕□≡⇌ ≠@ . \"I love arrays\"") }
            { inverse_row([Partition], No, "Inner function must be invertible", "⍜⊜□≡⇌ ≠@ . \"Hello World\"") }
            { inverse_row([Fold], No, "Inner function must be invertible", "⍜∧⊏(×10) [0 2] ↯2_3⇡6") }
            { inverse_row([Repeat], Optional, "Inner function must be invertible", "⍜⍥(×2). 5 1") }
            { inverse_row_impl(view!(<code>"⟨…|…|…⟩"</code>), No, "Switch function", "⍜⟨⊢|⊢⇌⟩(×10) 1 [1 2 3 4]") }
            { inverse_row([Now], No, "Times execution", "⍜now(&sl 0.005)") }
            { inverse_row([Sys(FOpen)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(FCreate)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(RunStream)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(TcpConnect)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(TcpAccept)], Optional, view!("Calls "<Prim prim=Sys(Close)/>" on handle"), None) }
            { inverse_row([Sys(FReadAllStr)], Optional, view!("Calls "<Prim prim=Sys(FWriteAll)/>), None) }
            { inverse_row([Sys(FReadAllBytes)], Optional, view!("Calls "<Prim prim=Sys(FWriteAll)/>), None) }
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
