// Springer-like journal article template for pandoc → typst.
// Invoked via -V template=…/typst-template.typ (see build_pdf.sh).

#let content-to-string(content) = {
  if content == none {
    ""
  } else if type(content) == str {
    content
  } else if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  } else {
    ""
  }
}

#let conf(
  title: none,
  subtitle: none,
  authors: (),
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  shorttitle: none,
  correspondence: none,
  cols: 1,
  margin: (x: 2.2cm, y: 2.2cm),
  paper: "a4",
  lang: "en",
  region: "US",
  font: ("Charter", "Times New Roman", "Georgia"),
  fontsize: 10pt,
  mathfont: none,
  codefont: ("Menlo", "Courier New"),
  headingfont: ("Helvetica Neue", "Helvetica", "Arial"),
  linestretch: 1.05,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  pagenumbering: "1",
  doc,
) = {
  let run-title = if shorttitle != none {
    shorttitle
  } else if title != none {
    title
  } else {
    none
  }

  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()

  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
    columns: cols,
    header: context {
      let n = counter(page).get().first()
      if n > 1 and run-title != none {
        set text(size: 8pt, font: headingfont, fill: luma(60))
        grid(
          columns: (1fr, auto),
          align: (left, right),
          [#emph(run-title)],
          [#counter(page).display()],
        )
        v(2pt)
        line(length: 100%, stroke: 0.4pt + luma(160))
      }
    },
    footer: context {
      let n = counter(page).get().first()
      if n == 1 and pagenumbering != none {
        align(center, text(size: 8.5pt, font: headingfont, fill: luma(70))[
          #counter(page).display()
        ])
      }
    },
  )

  set text(
    lang: lang,
    region: region,
    size: fontsize,
    font: font,
  )
  set par(
    justify: true,
    leading: linestretch * 0.65em,
    spacing: 0.7em,
    first-line-indent: 0pt,
  )

  show math.equation: set text(font: mathfont) if mathfont != none
  show raw: set text(font: codefont, size: 0.88em)

  set heading(numbering: sectionnumbering)

  let style-heading(it) = {
    set text(font: headingfont, fill: luma(20))
    set par(first-line-indent: 0pt, spacing: 0.45em)
    if it.level == 1 {
      // Journal-like: more air above sections, clear gap before body
      block(above: 1.85em, below: 0.8em, sticky: true)[
        #text(size: 11.5pt, weight: "bold", tracking: 0.02em)[#it.body]
        #v(3pt)
        #line(length: 100%, stroke: 0.5pt + luma(140))
      ]
    } else if it.level == 2 {
      block(above: 1.45em, below: 0.9em, sticky: true)[
        #text(size: 10.5pt, weight: "bold")[#it.body]
      ]
    } else {
      block(above: 1.2em, below: 0.5em, sticky: true)[
        #text(size: 10pt, weight: "semibold", style: "italic")[#it.body]
      ]
    }
  }

  // Track References section for hanging-indent bibliography look
  let in-refs = state("in-refs", false)
  show heading: it => {
    let label = content-to-string(it.body)
    if it.level == 1 and label.starts-with("References") {
      in-refs.update(true)
    } else if it.level == 1 {
      in-refs.update(false)
    }
    style-heading(it)
  }
  show par: it => context {
    if in-refs.get() {
      set text(size: 9pt)
      // Reconstruct so hanging-indent actually applies to bibliography paragraphs
      par(
        hanging-indent: 1.35em,
        first-line-indent: 0pt,
        leading: 0.58em,
        spacing: 0.42em,
        justify: true,
        it.body,
      )
    } else {
      it
    }
  }

  let link-fill = if linkcolor != none {
    rgb(content-to-string(linkcolor))
  } else {
    rgb("#1a4a6e")
  }
  show link: set text(fill: link-fill)
  show ref: set text(fill: if citecolor != none {
    rgb(content-to-string(citecolor))
  } else {
    luma(30)
  })
  show link: this => {
    if filecolor != none and type(this.dest) == label {
      text(this, fill: rgb(content-to-string(filecolor)))
    } else {
      text(this)
    }
  }

  // Booktabs-like tables (top / header / bottom rules only)
  set table(
    inset: (x: 4.5pt, y: 3.5pt),
    stroke: (x, y) => {
      if y == 0 {
        (
          top: 0.7pt + luma(40),
          bottom: 0.45pt + luma(40),
          rest: none,
        )
      } else {
        (rest: none)
      }
    },
    align: horizon,
  )
  show table: it => {
    block(below: 0.35em, {
      it
      line(length: 100%, stroke: 0.7pt + luma(40))
    })
  }
  show figure.where(kind: table): set figure.caption(position: top)
  show figure.where(kind: image): set figure.caption(position: bottom)
  show figure.caption: it => {
    set text(size: 8.5pt, font: headingfont)
    set align(left)
    it
  }
  show table.cell.where(y: 0): set text(weight: "bold", size: 8.5pt, font: headingfont)
  show table.cell: set text(size: 8.5pt)

  show raw.where(block: true): it => {
    set text(size: 8.2pt)
    set par(leading: 0.55em)
    block(
      width: 100%,
      inset: (x: 8pt, y: 7pt),
      fill: luma(248),
      stroke: (left: 1.5pt + luma(170)),
      breakable: true,
      it,
    )
  }

  if title != none {
    block(below: 1.1em, width: 100%)[
      #align(center)[
        #text(
          size: 14pt,
          weight: "bold",
          font: headingfont,
          tracking: 0.01em,
          hyphenate: false,
        )[
          #title
          #if thanks != none {
            footnote(thanks, numbering: "*")
            counter(footnote).update(n => n - 1)
          }
        ]
      ]

      #if subtitle != none {
        v(0.45em)
        align(center)[
          #text(size: 9.5pt, style: "italic", fill: luma(50))[#subtitle]
        ]
      }

      #if authors != none and authors != () {
        v(0.85em)
        align(center)[
          #text(size: 10.5pt, font: headingfont)[
            #authors.map(a => a.name).join([, ], last: [ & ])
          ]
        ]
        v(0.35em)
        let affils = authors
          .map(a => content-to-string(a.affiliation))
          .filter(s => s != none and s != "")
        if affils.len() > 0 {
          align(center)[
            #text(size: 8.5pt, fill: luma(55))[
              #affils.join([; ])
            ]
          ]
        }
        let emails = authors
          .map(a => content-to-string(a.email))
          .filter(s => s != none and s != "")
        if correspondence != none or emails.len() > 0 {
          v(0.25em)
          align(center)[
            #text(size: 8pt, fill: luma(55))[
              #if correspondence != none {
                correspondence
              } else {
                [Correspondence: #link("mailto:" + emails.first())[#emails.join([, ])]]
              }
            ]
          ]
        }
      }

      #if date != none {
        v(0.4em)
        align(center)[
          #text(size: 8.5pt, fill: luma(60))[#date]
        ]
      }

      #if abstract != none {
        v(0.9em)
        block(
          width: 100%,
          inset: (x: 0.65em, y: 0.55em),
          fill: luma(250),
          stroke: (left: 1.25pt + luma(120)),
        )[
          #set text(size: 9pt)
          #set par(leading: 0.62em, spacing: 0.55em, justify: true)
          #text(weight: "bold", font: headingfont, size: 9pt)[
            #if abstract-title != none { abstract-title } else { [Abstract] }
          ]
          #v(0.35em)
          #abstract
        ]
      }

      #if keywords != none and keywords != () {
        v(0.55em)
        block(width: 100%)[
          #set text(size: 8.5pt)
          #text(weight: "bold", font: headingfont)[Keywords]#h(0.4em)
          #text(fill: luma(40))[
            #keywords.join([; ])
          ]
        ]
      }
    ]
  }

  doc
}
