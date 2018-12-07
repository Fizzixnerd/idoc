# [idoc][]

idoc is a new extensible humane markup language. It compiles to HTML and LaTeX.
Its primary use case is as the markup language for Independent Learning in
Science (ILS), but it is flexible and can be extended to most needs easily
through its powerful *block* and *markup* constructs.

## Built-in Constructs

While most functionality in idoc is provided by blocks and markup, there are a
few built-in features that are common to most markup languages. These include
bold and italicized text, and links.

```idoc
*Bold text.*
_Italicized text._
<<https://fizzixnerd.com>>{A link with text}
```

Links come in three varieties, discussed below.

## Extensibility

Technically, idoc is more of a markup language framework than a language itself.
idoc documents is made up of sections. Each section contains paragraphs and
blocks. Paragraphs are composed of markup and text. Exactly what type of blocks
and markup the language supports is up to the user, with more being straight
forward to add to an existing language.

### Blocks

Blocks have a common structure.  They look like this:

```idoc
@blocktype
#Optional Block Title
---
Block contents go between the dashes.
---
[[#optionalId]]
```

The `blocktype` is a name which identifies the type of content the block will
contain. This means that restrictions on the type of content in each block can
be enforced; each blocktype has its own parser. For example, we can have a math block which is written in a LaTeX like language:

```idoc
@math
---
f(x) = \int_{-\infty}^x \frac{dt}{t ^ 2 + 1}
---
```

Another block represents a bibliography, written using Bibtex syntax:

```idoc
@bibliography
---
@book{myBook,
      author = "Nobody",
      title = "A book title",
      year = "2018"
}
---
```

Some blocks are simply to highlight or frame certain content:

```idoc
@warning
---
Do not confuse a homomorphism with a homeomorphism!
---
```

idoc is designed in such a way that a new blocktype can be added to a language
nearly effortlessly using a minimal amount of Haskell code. One simply defines
how to parse and render it, often using existing combinators and helpers.
However, full access to the text of the block is provided, should you wish to
add something truly custom. All of the blocks discussed so far are already
written and ready for you to use.

### Markup

If blocks are the "divs" of idoc, markup are the "spans". Markup are inline
decorators to text that are just as extensible as blocks. They look like this:

```idoc
%markuptype{contents}[[#optionalId]]
```

Here an example using a footnote:

```idoc
Here is a footnote.%footnote{And the footnote text!}[[#footnoteId]]
```

Like blocks, each markuptype has its own parser and renderer, making them fully
customizable. However, this will rarely be necessary, as idoc comes with most
markup types you would want anyways. All you have to do is include them in your
language.

## Links

idoc contains one more feature which sets it apart from more markup languages:
*restricted links*. In most markup languages, links are allowed anywhere and can
point to any other page. In idoc, they are only allowed within the constraints
of the current context. In creating your idoc language, you can completely
ignore this, and just allow all links if you'd like. Below with give an example
of why you might not want this.

### Back Links

Back links in idoc are links that refer to other idoc documents. For example,
you might have them refer to other idoc documents on your personal webpage. The
look like this:

```idoc
<</Path/To/Article#optionalId>>{optional text.}
```

Notice that unlike the links seen above (called "out links" in idoc paralance),
there is no protocol here. However, suppose you were building a website where
you wanted only to allow links to certain "previous" articles. This might be
beneficial if writing an online textbook, where you want to ensure you only
refer to material that the student has already seen. Using restricted links,
this becomes straightforward. ILS-flavoured idoc uses this feature in a similar
way, ensuring that articles proceed in a logically ordered manner. idoc can be
made to warn or outright reject articles which do not follow these rules.

### Internal Links

idoc also contains links to objects defined in the same document as the link.
These are called internal links. It is currently not possible to place
restrictions on these types of links. They look like:

```idoc
<<#idOfObject>>{optional text.}
```

## Conclusion

If you're looking for certain custom features in a markup language but don't
want to design one from scratch, idoc could be a good choice for you.

[idoc]: https://gitlab.com/matt.g.d.walker/idoc
