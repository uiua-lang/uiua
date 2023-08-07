use leptos::*;
use uiua::primitive::Primitive;

use crate::code::PrimCode;

#[component]
pub fn Technical(cx: Scope) -> impl IntoView {
    view! { cx,
        <h1>"Technical Details"</h1>

        <h2>"The Interpreter"</h2>
        <p>"The Uiua interpreter is written in Rust."</p>
        <p>"An entire file is first parsed into an abstract syntax tree. Then, each line is, one after the other, compiled to a simple bytecode and executed."</p>
        <p>"Built-in functions are implemented in Rust so they can be as fast as possible. User defined functions are passed around as chunks of bytecode."</p>

        <h2>"Arrays"</h2>
        <p>"Values on the stack are implemented as Rust "<code>"enum"</code>"s, where each variant contains a different array type."</p>
        <p>"While the language itself only has 3 types, the interpreter has 1 extra: a byte array. IO streams and operations which have boolean results return byte arrays for space efficiency."</p>
        <h3>"Fill Values"</h3>
        <p>"Each array type uses a different fill value:"</p>
        <ul>
            <li>"Numbers are filled with "<code>"NaN"</code>"s."</li>
            <li>"Characters are filled with the null character "<code>"\\0"</code>"."</li>
            <li>"Functions are filled with "<PrimCode prim=Primitive::Noop/>"."</li>
            <li>"Regrettably, unlike the other three types, Rust's "<code>"u8"</code>" type does not have a suitable value to use as fill. Because of this, the actual element type of byte arrays is a Rust "<code>"enum"</code>" which has a variant for the actual value and a variant for the fill state. While this technically only adds a single bit of information to each element, alignment demands that every byte element use 2 bytes of memory. This means that any file read into memory will take up twice as much space as it does on disk. While this is a 4x improvement on potentially storing every byte as an "<code>"f64"</code>", it is 2x less space efficent than if the fill states were stored seperately from the values. However, I have deemed the runtime overhead and increased code complexity this would introduce to not be worth it."</li>
        </ul>

        <h2>"The Website"</h2>
        <p>"The Uiua website is written using the "<a href="https://leptos.dev/">Leptos</a>" framework and hosted on GitHub pages."</p>
        <p>"Leptos compiles to webassembly, which allows the entire Uiua interpreter to be compiled and used by the site's editor."</p>
        <p>"The online editor is implemented as a "<code>"contenteditable"</code>" div with lots of custom behaviors."</p>
    }
}
