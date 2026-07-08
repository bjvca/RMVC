#!/usr/bin/env python3
"""Render the policy briefs to branded PDFs (IFPRI style) via pandoc + WeasyPrint."""
import re
import subprocess
from pathlib import Path
from weasyprint import HTML

HERE = Path(__file__).resolve().parent
BRIEFS = ["processor_brief", "government_brief"]

# IFPRI theme palette
GREEN, GREEN_DK = "#62BA45", "#3E7D2C"
TEAL, BLUE, SLATE, ORANGE = "#00AE9A", "#007DB3", "#44546A", "#F7921E"
GREY_BG, BORDER = "#F5F7F9", "#D5DBE0"

CSS = f"""
@page {{
  size: A4;
  margin: 1.5cm 1.5cm 1.7cm 1.5cm;
  @bottom-left {{
    content: "IFPRI Policy Brief  ·  July 2026";
    font-family: Lato; font-size: 7.5pt; color: {SLATE};
  }}
  @bottom-right {{
    content: counter(page) " of " counter(pages);
    font-family: Lato; font-size: 7.5pt; color: {SLATE};
  }}
}}
@page {{ @top-center {{
  content: "TECHNOLOGY, TRANSPARENCY, AND MARKETS FOR QUALITY IN UGANDAN DAIRY";
  font-family: Lato; font-size: 6.8pt; letter-spacing: .06em; color: {BORDER};
}} }}
@page:first {{ @top-center {{ content: ""; }} }}

* {{ box-sizing: border-box; }}
html {{ font-family: Lato, "Liberation Sans", sans-serif; color: #222; }}
body {{ font-size: 9.3pt; line-height: 1.42; margin: 0; }}

/* ---- Masthead ---- */
.masthead {{
  display: flex; justify-content: space-between; align-items: center;
  border-bottom: 3px solid {GREEN}; padding-bottom: 7px; margin-bottom: 2px;
}}
.brand {{ display: flex; align-items: center; gap: 9px; }}
.logo {{
  background: {GREEN}; color: #fff; font-weight: 900; font-size: 19pt;
  letter-spacing: .04em; padding: 3px 9px 2px; border-radius: 3px; line-height: 1;
}}
.brand .org {{ font-size: 7.6pt; color: {SLATE}; line-height: 1.25; max-width: 190px; }}
.brand .org b {{ color: {GREEN_DK}; }}
.doctype {{ text-align: right; }}
.doctype .k {{ font-weight: 800; font-size: 10.5pt; color: {SLATE}; letter-spacing: .12em; }}
.doctype .d {{ font-size: 7.6pt; color: {SLATE}; }}

/* ---- Title block ---- */
h1 {{ font-size: 19pt; line-height: 1.1; color: {SLATE}; margin: 12px 0 3px; font-weight: 800; }}
.subtitle {{ font-size: 11pt; color: {GREEN_DK}; font-weight: 700; margin: 0 0 4px; }}
.provenance {{ font-size: 7.8pt; color: #6b7178; font-style: italic; margin: 0 0 4px;
  padding-bottom: 8px; border-bottom: 1px solid {BORDER}; }}

/* ---- Sections ---- */
h2 {{ font-size: 11.5pt; color: {BLUE}; font-weight: 800; margin: 13px 0 4px;
  padding-left: 8px; border-left: 4px solid {GREEN}; }}
h3 {{ font-size: 9.6pt; color: {TEAL}; font-weight: 800; margin: 9px 0 2px; }}
p {{ margin: 4px 0; }}
strong {{ color: {SLATE}; }}
hr {{ display: none; }}

/* ---- Recommendation lead-ins (bold sentence starts) ---- */
h2 + p strong:first-child {{ color: {GREEN_DK}; }}

/* ---- Tables (price simulation) ---- */
table {{ border-collapse: collapse; width: 100%; margin: 7px 0; font-size: 8.7pt; }}
th {{ background: {GREEN}; color: #fff; text-align: left; padding: 4px 8px; font-weight: 700; }}
td {{ padding: 3.5px 8px; border-bottom: 1px solid {BORDER}; }}
tr:nth-child(even) td {{ background: {GREY_BG}; }}
table strong {{ color: {GREEN_DK}; }}

/* keep headings with following text */
h1, h2, h3 {{ break-after: avoid; }}
table, tr {{ break-inside: avoid; }}
"""

MASTHEAD = f"""
<div class="masthead">
  <div class="brand">
    <span class="logo">IFPRI</span>
    <span class="org"><b>International Food Policy<br>Research Institute</b><br>
      A CGIAR Research Center</span>
  </div>
  <div class="doctype"><div class="k">POLICY BRIEF</div><div class="d">July 2026</div></div>
</div>
"""

def split_front_matter(md: str):
    """Pull title (# ), subtitle (### ), provenance (italic), body (after first ---)."""
    title = re.search(r"^#\s+(.+)$", md, re.M)
    subtitle = re.search(r"^###\s+(.+)$", md, re.M)
    prov = re.search(r"^\*(.+)\*\s*$", md, re.M)
    body = md.split("\n---\n", 1)[1] if "\n---\n" in md else md
    return (title.group(1).strip() if title else "",
            subtitle.group(1).strip() if subtitle else "",
            prov.group(1).strip() if prov else "",
            body.strip())

def md_to_html(body_md: str) -> str:
    out = subprocess.run(
        ["pandoc", "-f", "markdown+pipe_tables", "-t", "html", "--no-highlight"],
        input=body_md, capture_output=True, text=True, check=True)
    return out.stdout

def build(name: str):
    md = (HERE / f"{name}.md").read_text()
    title, subtitle, prov, body = split_front_matter(md)
    body_html = md_to_html(body)
    html = f"""<!doctype html><html><head><meta charset="utf-8">
<style>{CSS}</style></head><body>
{MASTHEAD}
<h1>{title}</h1>
<div class="subtitle">{subtitle}</div>
<div class="provenance">{prov}</div>
{body_html}
</body></html>"""
    (HERE / f"{name}.html").write_text(html)  # keep for inspection
    HTML(string=html, base_url=str(HERE)).write_pdf(str(HERE / f"{name}.pdf"))
    print(f"wrote {name}.pdf")

if __name__ == "__main__":
    for b in BRIEFS:
        build(b)
