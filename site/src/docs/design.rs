use leptos::*;
use uiua::primitive::Primitive;

use crate::{code::PrimCode, editor::Editor};

#[component]
pub fn Design(cx: Scope) -> impl IntoView {
    use Primitive::*;
    view! { cx,
        <h1>"Design"</h1>
        <p>"This page explains the reasons for some of Uiua's design decisions."</p>
        <p>"It serves as a "<a href="https://news.knowledia.com/US/en/articles/more-software-projects-need-defenses-of-design-85ea9e23ffd85f5fde5a2d3d42001393cbce169a">"defense of design"</a>"."</p>
        <br/>
        <h2>"Stack Orientation"</h2>
        <hr/>
        <h3>"Combinators"</h3>
        <p>"When I first started developing Uiua, it was neither stack-oriented nor array-oriented. What it "<em>"did"</em>" focus a lot on was "<em>"combinators"</em>". I had this whole heirarchy of language-level operators that let you construct arbitrarily complex combinators relatively succinctly."</p>
        <p>"I discovered what a lot of others have discovered when delving deep into tacit code: it's really hard to read and write and reason about."</p>
        <p>"Eventually, I moved to a stack-oriented model and discovered that you can write almost any 1 or 2 argument combinator with just "<PrimCode prim=Dup/>", "<PrimCode prim=Over/>", and "<PrimCode prim=Flip/>"."</p>
        <p>"Of course, I also made the discovery that juggling 3 or more values on the stack also imposes a high cognitive load on the developer, which is why, unlike some other stack-oriented languages, Uiua does not have a rotate function."</p>
        <p>"Thankfully, the array paradigm saved the day with the idea of dfns, which I expanded to be able to take way more arguments, so you can still write complex yet concise combinators."</p>
        <br/>
        <h3>"Expressions"</h3>
        <p>"Long tacit expressions in most array languages can get very unwieldy. Because binary operations are infix, you have to parse the tree structure in your head before you can start determining the order of operations."</p>
        <p>"For example, in BQN, you can trim matches from the beginning of a string with "<a style="text-decoration: none;" href="https://mlochbaum.github.io/bqncrate/?q=Remove%20cells%20that%20appear%20in%20x%20from%20beginning%20of%20y#"><code>"x(∧`∘∊˜¬⊸/⊢)y"</code></a>". "</p>
        <p>"In contrast, here is there equivalent in Uiua, implemented the same way:"</p>
        <Editor example="Trim ← ‡¬\\×∊,\n"/>
        <p>
            "You'll notice that stack orientation simplifies the expression in a few ways:"
            <ul>
                <li>"There is no Uiua code corresponding to the BQN combinators "<code>"∘"</code>" and "<code>"⊸"</code>" and the identity function "<code>"⊢"</code>". Function composition is implicit."</li>
                <li>"Functions are executed right-to-left instead of in a right-to-left tree ordering."</li>
                <li>"The expression does not require "<code>"()"</code>"s. In fact, no Uiua expression requires explicit grouping. "<code>"()"</code>" is used to make inline functions instead."</li>
            </ul>
        </p>
        <p>"I think this clarity makes writing long tacit expression much more workable."</p>
        <br/>
        <h2>"The Flat Array Model"</h2>
        <hr/>
        <p>"Veterans of existing array languages may view Uiua's flat array model as a step backwards."</p>
        <p>"All modern array languages allow both heterogenous arrays and nested arrays. Uiua, however, requires that all elements of an array be of the same type, and it forbids the nesting of arrays."</p>
        <p>"Uiua forgoes these features for the sake of simplicity, both in the implementation of the interpreter and in the language itself. It is easier to reason about both the semantics and performance of code when arrays are flat and homogenous."</p>
        <p>"Uiua allows something resembling nested arrays with its fill elements. I find that filled arrays are sufficient for most applications where I would want nested arrays."</p>
        <p>"Array homogeneity it less limiting in Uiua than other array languages because while types cannot be mixed in an array, they "<em>"can"</em>" be mixed on the stack. Arrays which are associated but which have different types can be passed around together relatively easily."</p>
        <br/>
        <h2>"The Glyphs"</h2>
        <hr/>
        <p>"Most of Uiua's glyphs were chosen for one of a few reasons:"</p>
        <ul>
            <li>"It is a common mathematical symbol, such as "<PrimCode prim=Add/>", "<PrimCode prim=Sub/>", and "<PrimCode prim=Pi/>"."</li>
            <li>"It is a very commonly used function and should create little line noise, such as "<PrimCode prim=Dup/>" and "<PrimCode prim=Debug/>"."</li>
            <li>"It is used in other array languages, such as "<PrimCode prim=Reduce/>", "<PrimCode prim=Grade/>", and "<PrimCode prim=Transpose/>"."</li>
            <li>"It kind of reminds me of what it does. Some of my favorites are "<PrimCode prim=Table/>", "<PrimCode prim=Reshape/>", "<PrimCode prim=Rotate/>", "<PrimCode prim=Deshape/>", "<PrimCode prim=Find/>", and "<PrimCode prim=Recur/>"."</li>
            <li>"Its function is kind of abstract, but there are other related functions, so they all use related glyphs. For example, "<PrimCode prim=Fold/>" in relation to "<PrimCode prim=Reduce/>", and also all the indexing/finding/grouping functions like"<PrimCode prim=Indices/>", "<PrimCode prim=Classify/>", "<PrimCode prim=Group/>", etc."</li>
        </ul>
        <br/>
        <h2>"No Local Variables"</h2>
        <hr/>
        <p>"While Uiua does technically have local variables in the form of dfn arguments, they are very limited in that they can only be used in the dfn body and can only be single-letter names."</p>
        <p>"Forbidding general local variables has a few benefits:"</p>
        <ul>
            <li>"I don't have to implement them (score!)"</li>
            <li>"It forces you to write beautiful tacit code, which I would argue Uiua enables better than almost any other programming language."</li>
            <li>"It frees you from the burden of naming things."</li>
        </ul>
    }
}
