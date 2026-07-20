#!/usr/bin/env python3
"""
Convert a LyX 2.5 (format 643) file to LaTeX (.tex).

Handles: sections, subsections, paragraphs, footnotes, citations,
cross-references, figures, tables (via ERT), math (inline & display),
bold/italic formatting, bibliography, Quotes insets, Notes, ERT blocks,
Flex Chunks (knitr), and \Sexpr inline R expressions.
"""

import re
import sys
import os


class LyxToLatex:
    def __init__(self, lyx_path):
        with open(lyx_path, 'r', encoding='utf-8') as f:
            self.lines = f.readlines()
        self.pos = 0
        self.preamble_extra = []
        self.body_lines = []
        self.in_document = False
        self.bib_file = ""
        self.bib_style = ""
        self.doc_class = "article"
        self.use_natbib = False
        self.use_hyperref = False
        self.hyperref_opts = ""
        self.preamble_user = []
        self.title_content = ""
        self.abstract_paragraphs = []
        self.paper_dir = os.path.dirname(os.path.abspath(lyx_path))
        self._bold_open = False
        self._italic_open = False

    def line(self):
        if self.pos < len(self.lines):
            return self.lines[self.pos].rstrip('\n')
        return None

    def advance(self):
        self.pos += 1

    def peek(self, offset=1):
        idx = self.pos + offset
        if idx < len(self.lines):
            return self.lines[idx].rstrip('\n')
        return None

    def skip_blank(self):
        while self.pos < len(self.lines) and self.line().strip() == '':
            self.advance()

    def parse(self):
        """Main parse loop."""
        # Parse header
        self.parse_header()
        # Parse body
        self.parse_body()
        return self.build_output()

    def parse_header(self):
        """Parse \begin_header ... \end_header"""
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            if l.strip() == '\\begin_header':
                self.advance()
                self._parse_header_content()
                break
            self.advance()

    def _parse_header_content(self):
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls == '\\end_header':
                self.advance()
                return
            if ls.startswith('\\textclass '):
                self.doc_class = ls.split(None, 1)[1]
            elif ls.startswith('\\begin_preamble'):
                self.advance()
                self._collect_preamble()
                continue
            elif ls == '\\use_hyperref true':
                self.use_hyperref = True
            elif ls.startswith('\\pdf_quoted_options'):
                self.hyperref_opts = ls.split('"')[1] if '"' in ls else ''
            elif ls.startswith('\\cite_engine '):
                engine = ls.split(None, 1)[1]
                if 'natbib' in engine:
                    self.use_natbib = True
            elif ls.startswith('\\cite_engine_type '):
                pass  # authoryear handled via natbib
            elif ls.startswith('\\biblio_style '):
                self.bib_style = ls.split(None, 1)[1]
            self.advance()

    def _collect_preamble(self):
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            if l.strip() == '\\end_preamble':
                self.advance()
                return
            self.preamble_user.append(l.rstrip('\n'))
            self.advance()

    def parse_body(self):
        """Parse \begin_body ... \end_body"""
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            if l.strip() == '\\begin_body':
                self.advance()
                self._parse_body_content()
                break
            self.advance()

    def _parse_body_content(self):
        self._current_list_env = None  # Track open list environment
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls == '\\end_body':
                self._close_list_env()
                self.advance()
                return
            if ls.startswith('\\begin_layout '):
                layout_type = ls[len('\\begin_layout '):].strip()
                # Handle list environment transitions
                if layout_type in ('Enumerate', 'Itemize'):
                    env_name = 'enumerate' if layout_type == 'Enumerate' else 'itemize'
                    if self._current_list_env != env_name:
                        self._close_list_env()
                        self._current_list_env = env_name
                        self.body_lines.append('\\begin{' + env_name + '}\n')
                else:
                    self._close_list_env()
                self.advance()
                content = self.parse_layout(layout_type)
                self._emit_layout(layout_type, content)
            else:
                self.advance()

    def _close_list_env(self):
        if self._current_list_env:
            self.body_lines.append('\\end{' + self._current_list_env + '}\n')
            self._current_list_env = None

    def parse_layout(self, layout_type):
        """Parse content within a \begin_layout ... \end_layout block.
        Returns the text content with LaTeX markup."""
        parts = []
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()

            if ls == '\\end_layout':
                self.advance()
                return ''.join(parts)

            if ls.startswith('\\begin_inset '):
                inset_type = ls[len('\\begin_inset '):].strip()
                self.advance()
                inset_text = self.parse_inset(inset_type)
                parts.append(inset_text)
            elif ls.startswith('\\begin_layout '):
                # Nested layout - shouldn't normally happen at this level
                nested_type = ls[len('\\begin_layout '):].strip()
                self.advance()
                nested = self.parse_layout(nested_type)
                parts.append(nested)
            elif ls == '\\series bold':
                parts.append('\\textbf{')
                self._bold_open = True
                self.advance()
            elif ls == '\\series default':
                if self._bold_open:
                    parts.append('}')
                    self._bold_open = False
                self.advance()
            elif ls == '\\emph on':
                parts.append('\\textit{')
                self._italic_open = True
                self.advance()
            elif ls == '\\emph default':
                if self._italic_open:
                    parts.append('}')
                    self._italic_open = False
                self.advance()
            elif ls == '\\backslash':
                self.advance()
                # Next line is the continuation
                if self.pos < len(self.lines):
                    next_l = self.line().rstrip('\n')
                    # Check if it's just a newline command (empty or whitespace after backslash)
                    parts.append('\\' + next_l.lstrip(' '))
                    self.advance()
            elif ls == '\\align center':
                parts.append('% align center\n')
                self.advance()
            elif ls.startswith('\\paragraph_spacing ') or ls.startswith('\\labelwidthstring'):
                self.advance()  # skip formatting directives
            elif ls == '\\noindent':
                parts.append('\\noindent ')
                self.advance()
            elif ls == '\\emph off':
                # emph off without a matching emph on - skip
                self.advance()
            elif any(ls.startswith(prefix) for prefix in [
                '\\family ', '\\shape ', '\\size ', '\\bar ',
                '\\strikeout ', '\\xout ', '\\uuline ', '\\uwave ',
                '\\noun ', '\\color ', '\\nospellcheck ',
                '\\lang ', '\\numeric ', '\\series '
            ]):
                # LyX formatting directives - skip
                # Note: \series bold and \series default are handled above;
                # other \series values (e.g., medium) are just skipped
                self.advance()
            else:
                # Regular text line
                text = l.rstrip('\n')
                # In LyX format, continuation lines start with a single space
                # which serves as the word separator. We preserve it.
                # Only strip the space if this is the very first content
                # (i.e., parts is empty and text is just whitespace)
                if not parts and text.strip() == '':
                    self.advance()
                    continue
                parts.append(text)
                self.advance()

        return ''.join(parts)

    def parse_inset(self, inset_type):
        """Parse \begin_inset TYPE ... \end_inset and return LaTeX equivalent."""

        if inset_type.startswith('CommandInset citation'):
            return self._parse_citation_inset()
        elif inset_type.startswith('CommandInset label'):
            return self._parse_label_inset()
        elif inset_type.startswith('CommandInset ref'):
            return self._parse_ref_inset()
        elif inset_type.startswith('CommandInset bibtex'):
            return self._parse_bibtex_inset()
        elif inset_type.startswith('Float figure'):
            return self._parse_float_inset('figure')
        elif inset_type.startswith('Float table'):
            return self._parse_float_inset('table')
        elif inset_type.startswith('Graphics'):
            return self._parse_graphics_inset()
        elif inset_type.startswith('Formula'):
            return self._parse_formula_inset(inset_type)
        elif inset_type.startswith('Foot'):
            return self._parse_foot_inset()
        elif inset_type.startswith('Caption Standard'):
            return self._parse_caption_inset()
        elif inset_type.startswith('ERT'):
            return self._parse_ert_inset()
        elif inset_type.startswith('Flex Chunk'):
            return self._parse_flex_chunk_inset()
        elif inset_type.startswith('Quotes'):
            return self._parse_quotes_inset(inset_type)
        elif inset_type.startswith('Note Note') or inset_type.startswith('Note Comment'):
            return self._skip_inset()  # Notes are not rendered
        elif inset_type.startswith('Newpage'):
            self._skip_to_end_inset()
            return '\\newpage\n'
        elif inset_type.startswith('Preview'):
            return self._parse_preview_inset()
        elif inset_type.startswith('Argument '):
            return self._skip_inset()  # Arguments handled by parent
        elif inset_type.startswith('space ~'):
            self._skip_to_end_inset()
            return '~'
        elif inset_type.startswith('Separator'):
            self._skip_to_end_inset()
            return ''
        else:
            # Unknown inset - try to extract text content
            return self._parse_generic_inset()

    def _skip_inset(self):
        """Skip to \end_inset, consuming everything."""
        depth = 1
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_inset '):
                depth += 1
            elif ls == '\\end_inset':
                depth -= 1
                if depth == 0:
                    self.advance()
                    return ''
            self.advance()
        return ''

    def _skip_to_end_inset(self):
        """Skip to matching \end_inset."""
        depth = 1
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_inset '):
                depth += 1
            elif ls == '\\end_inset':
                depth -= 1
                if depth == 0:
                    self.advance()
                    return
            self.advance()

    def _parse_citation_inset(self):
        """Parse CommandInset citation."""
        cmd = 'citep'
        key = ''
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls == '\\end_inset':
                self.advance()
                break
            if ls.startswith('LatexCommand '):
                cmd = ls.split(None, 1)[1]
            elif ls.startswith('key "'):
                key = ls.split('"')[1]
            self.advance()
        return '\\' + cmd + '{' + key + '}'

    def _parse_label_inset(self):
        """Parse CommandInset label."""
        name = ''
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls == '\\end_inset':
                self.advance()
                break
            if ls.startswith('name "'):
                name = ls.split('"')[1]
            self.advance()
        return '\\label{' + name + '}'

    def _parse_ref_inset(self):
        """Parse CommandInset ref."""
        reference = ''
        cmd = 'ref'
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls == '\\end_inset':
                self.advance()
                break
            if ls.startswith('reference "'):
                reference = ls.split('"')[1]
            elif ls.startswith('LatexCommand '):
                cmd = ls.split(None, 1)[1]
            self.advance()
        return '\\' + cmd + '{' + reference + '}'

    def _parse_bibtex_inset(self):
        """Parse CommandInset bibtex."""
        bibfiles = ''
        options = ''
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls == '\\end_inset':
                self.advance()
                break
            if ls.startswith('bibfiles "'):
                bibfiles = ls.split('"')[1]
            elif ls.startswith('options "'):
                options = ls.split('"')[1]
            self.advance()
        self.bib_file = bibfiles
        self.bib_style = options if options else self.bib_style
        return '\\bibliographystyle{' + self.bib_style + '}\n\\bibliography{' + bibfiles + '}'

    def _parse_float_inset(self, float_type):
        """Parse Float figure/table."""
        parts = ['\\begin{' + float_type + '}[htbp]\n']
        # Skip to content
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_layout '):
                layout_type = ls[len('\\begin_layout '):].strip()
                self.advance()
                content = self.parse_layout(layout_type)
                # Handle centering
                if '% align center' in content:
                    content = content.replace('% align center\n', '\\centering\n')
                parts.append(content + '\n')
            elif ls == '\\end_inset':
                self.advance()
                break
            else:
                self.advance()
        parts.append('\\end{' + float_type + '}\n')
        return '\n'.join(['']) + ''.join(parts)

    def _parse_graphics_inset(self):
        """Parse Graphics inset."""
        filename = ''
        scale = ''
        width = ''
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls == '\\end_inset':
                self.advance()
                break
            if ls.startswith('filename '):
                filename = ls.split(None, 1)[1]
            elif ls.startswith('scale '):
                scale = ls.split(None, 1)[1]
            elif ls.startswith('width '):
                width = ls.split(None, 1)[1]
            self.advance()

        # Resolve the filename relative to the paper directory
        if filename.startswith('../'):
            # Resolve relative to the LyX file location
            abs_path = os.path.normpath(os.path.join(self.paper_dir, filename))
            # Check if we have a local copy in figures/
            basename = os.path.basename(filename)
            local_fig = os.path.join(self.paper_dir, 'figures', basename)
            if os.path.exists(local_fig):
                filename = 'figures/' + basename
            else:
                filename = os.path.relpath(abs_path, self.paper_dir)

        # Prefer PNG over EPS for pandoc compatibility
        if filename.endswith('.eps'):
            png_alt = filename.replace('.eps', '.png')
            if os.path.exists(os.path.join(self.paper_dir, png_alt)):
                filename = png_alt

        opts = []
        if width:
            # LyX uses formats like "90text%" meaning 0.9\textwidth
            import re as _re
            m = _re.match(r'(\d+)text%', width)
            if m:
                frac = int(m.group(1)) / 100.0
                opts.append(f'width={frac}\\textwidth')
            elif 'col%' in width:
                m2 = _re.match(r'(\d+)col%', width)
                if m2:
                    frac = int(m2.group(1)) / 100.0
                    opts.append(f'width={frac}\\columnwidth')
                else:
                    opts.append('width=' + width)
            else:
                opts.append('width=' + width)
        elif scale:
            # Convert scale percentage to fraction of textwidth
            try:
                s = int(scale) / 100.0
                if s <= 1:
                    opts.append(f'width={s}\\textwidth')
                else:
                    opts.append(f'width=\\textwidth')
            except ValueError:
                opts.append('width=0.8\\textwidth')
        else:
            opts.append('width=0.8\\textwidth')

        opt_str = ','.join(opts)
        return f'\\includegraphics[{opt_str}]{{{filename}}}'

    def _parse_formula_inset(self, inset_type):
        """Parse Formula inset - inline or display math."""
        # inset_type may be "Formula $...$" for inline or just "Formula" for display
        rest = inset_type[len('Formula'):].strip()

        if rest:
            # Inline formula: the formula is on the same line as begin_inset
            # e.g., \begin_inset Formula $m$
            formula = rest
            self._skip_to_end_inset()
            return formula
        else:
            # Display formula: content is on subsequent lines
            formula_lines = []
            while self.pos < len(self.lines):
                l = self.line()
                if l is None:
                    break
                ls = l.strip()
                if ls == '\\end_inset':
                    self.advance()
                    break
                formula_lines.append(l.rstrip('\n'))
                self.advance()
            return '\n'.join(formula_lines)

    def _parse_foot_inset(self):
        """Parse Foot inset (footnote)."""
        # Skip status line
        parts = []
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_layout '):
                layout_type = ls[len('\\begin_layout '):].strip()
                self.advance()
                content = self.parse_layout(layout_type)
                parts.append(content)
            elif ls == '\\end_inset':
                self.advance()
                break
            else:
                self.advance()
        return '\\footnote{' + ' '.join(parts) + '}'

    def _parse_caption_inset(self):
        """Parse Caption Standard inset."""
        parts = []
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_layout '):
                layout_type = ls[len('\\begin_layout '):].strip()
                self.advance()
                content = self.parse_layout(layout_type)
                parts.append(content)
            elif ls == '\\end_inset':
                self.advance()
                break
            else:
                self.advance()
        return '\\caption{' + ' '.join(parts) + '}\n'

    def _parse_ert_inset(self):
        """Parse ERT (Evil Red Text = raw LaTeX) inset."""
        parts = []
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_layout '):
                layout_type = ls[len('\\begin_layout '):].strip()
                self.advance()
                content = self.parse_layout(layout_type)
                parts.append(content)
            elif ls == '\\end_inset':
                self.advance()
                break
            else:
                self.advance()
        return '\n'.join(parts)

    def _parse_flex_chunk_inset(self):
        """Parse Flex Chunk (knitr) inset - skip it (code chunks are for R processing)."""
        # We'll skip the chunk entirely as the Sexpr expressions in ERT will be handled separately
        return self._skip_inset()

    def _parse_quotes_inset(self, inset_type):
        """Parse Quotes inset."""
        self._skip_to_end_inset()
        if 'eld' in inset_type:  # English left double
            return '``'
        elif 'erd' in inset_type:  # English right double
            return "''"
        elif 'els' in inset_type:  # English left single
            return '`'
        elif 'ers' in inset_type:  # English right single
            return "'"
        return '"'

    def _parse_preview_inset(self):
        """Parse Preview inset - extract inner content."""
        parts = []
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_layout '):
                layout_type = ls[len('\\begin_layout '):].strip()
                self.advance()
                content = self.parse_layout(layout_type)
                parts.append(content)
            elif ls == '\\end_inset':
                self.advance()
                break
            else:
                self.advance()
        return '\n'.join(parts)

    def _parse_generic_inset(self):
        """Parse unknown inset type - try to extract text."""
        parts = []
        while self.pos < len(self.lines):
            l = self.line()
            if l is None:
                break
            ls = l.strip()
            if ls.startswith('\\begin_layout '):
                layout_type = ls[len('\\begin_layout '):].strip()
                self.advance()
                content = self.parse_layout(layout_type)
                parts.append(content)
            elif ls == '\\end_inset':
                self.advance()
                break
            else:
                self.advance()
        return ' '.join(parts)

    def _emit_layout(self, layout_type, content):
        """Convert a layout block to LaTeX and add to body."""
        content = content.strip()
        if not content and layout_type not in ('Standard',):
            return

        if layout_type == 'Title':
            # The Title layout may contain \author{...} from a Preview/ERT inset
            # followed by the actual title text. Separate them.
            import re as _re
            # Extract \author{...} command (with nested braces)
            author_match = _re.search(r'(\\author\{.*\})', content, _re.DOTALL)
            if author_match:
                author_cmd = author_match.group(1)
                # Remove the author command from content to get the title
                title_text = content[:author_match.start()] + content[author_match.end():]
                title_text = title_text.strip()
                self.title_content = title_text
                self.body_lines.append('\\title{' + title_text + '}\n')
                self.body_lines.append(author_cmd + '\n')
            else:
                self.title_content = content
                self.body_lines.append('\\title{' + content + '}\n')
            return
        elif layout_type == 'Author':
            self.body_lines.append('\\author{' + content + '}\n')
            return
        elif layout_type == 'Date':
            self.body_lines.append('\\date{' + content + '}\n')
            return
        elif layout_type == 'Abstract':
            self.abstract_paragraphs.append(content)
            return
        elif layout_type == 'Section':
            self.body_lines.append('\\section{' + content + '}\n')
            return
        elif layout_type == 'Section*':
            self.body_lines.append('\\section*{' + content + '}\n')
            return
        elif layout_type == 'Subsection':
            self.body_lines.append('\\subsection{' + content + '}\n')
            return
        elif layout_type == 'Subsection*':
            self.body_lines.append('\\subsection*{' + content + '}\n')
            return
        elif layout_type == 'Subsubsection':
            self.body_lines.append('\\subsubsection{' + content + '}\n')
            return
        elif layout_type == 'Subsubsection*':
            self.body_lines.append('\\subsubsection*{' + content + '}\n')
            return
        elif layout_type in ('Standard', 'Plain Layout'):
            if content:
                self.body_lines.append(content + '\n\n')
            return
        elif layout_type == 'Enumerate' or layout_type == 'Itemize':
            # These need wrapping but LyX handles them differently
            self.body_lines.append('\\item ' + content + '\n')
            return
        else:
            if content:
                self.body_lines.append(content + '\n\n')

    def build_output(self):
        """Build the final LaTeX document."""
        out = []
        out.append('\\documentclass{' + self.doc_class + '}\n')
        out.append('\\usepackage[utf8]{inputenc}\n')
        out.append('\\usepackage{graphicx}\n')
        out.append('\\usepackage{amsmath}\n')
        out.append('\\usepackage{amssymb}\n')
        if self.use_natbib:
            out.append('\\usepackage[authoryear]{natbib}\n')
        if self.use_hyperref:
            if self.hyperref_opts:
                out.append('\\usepackage[' + self.hyperref_opts + ']{hyperref}\n')
            else:
                out.append('\\usepackage{hyperref}\n')

        # User preamble
        for line in self.preamble_user:
            # Skip duplicate usepackage commands
            if '\\usepackage{amsmath}' in line or '\\usepackage{amssymb}' in line:
                continue
            out.append(line + '\n')

        out.append('\n\\begin{document}\n\n')

        # Emit \title{} and \author{} commands before \maketitle
        for line in self.body_lines:
            if line.startswith('\\title{') or line.startswith('\\author{') or line.startswith('\\date{'):
                out.append(line)

        # Title and abstract
        if self.title_content:
            out.append('\\maketitle\n\n')

        if self.abstract_paragraphs:
            out.append('\\begin{abstract}\n')
            for p in self.abstract_paragraphs:
                if p.strip():
                    out.append(p + '\n\n')
            out.append('\\end{abstract}\n\n')

        # Body (skip title/author/date since already emitted)
        for line in self.body_lines:
            if line.startswith('\\title{') or line.startswith('\\author{') or line.startswith('\\date{'):
                continue
            out.append(line)

        out.append('\n\\end{document}\n')
        return ''.join(out)


def clean_tex(tex):
    """Post-process the generated TeX to fix common issues."""
    # Fix double-backslash newlines that got mangled
    # In LyX, \\ is represented as two separate \backslash lines
    # Our parser produces \\ which is correct for table newlines

    # Fix empty bold/italic toggles
    tex = re.sub(r'\\textbf\{\s*\}', '', tex)
    tex = re.sub(r'\\textit\{\s*\}', '', tex)

    # Fix mismatched bold/italic (unclosed)
    # Count braces in textbf/textit
    # This is a simple heuristic
    tex = tex.replace('\\textbf{\\textbf{', '\\textbf{')
    tex = tex.replace('\\textit{\\textit{', '\\textit{')

    # Remove multiple consecutive blank lines
    tex = re.sub(r'\n{4,}', '\n\n\n', tex)

    # Fix Sexpr: inside \Sexpr{...} expressions, replace LaTeX quotes with "
    def fix_sexpr_quotes(match):
        expr = match.group(0)
        expr = expr.replace("``", '"').replace("''", '"')
        return expr
    tex = re.sub(r'\\Sexpr\{[^}]*(?:\{[^}]*\}[^}]*)*\}', fix_sexpr_quotes, tex)

    return tex


def main():
    lyx_path = sys.argv[1] if len(sys.argv) > 1 else '/home/claude/workspace/RMVC/paper/paper.lyx'
    converter = LyxToLatex(lyx_path)
    tex = converter.parse()
    tex = clean_tex(tex)

    out_path = lyx_path.replace('.lyx', '.tex')
    if len(sys.argv) > 2:
        out_path = sys.argv[2]

    with open(out_path, 'w', encoding='utf-8') as f:
        f.write(tex)

    print(f"Wrote {out_path} ({len(tex)} bytes)")
    return out_path


if __name__ == '__main__':
    main()
