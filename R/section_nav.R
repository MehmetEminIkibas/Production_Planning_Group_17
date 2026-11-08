# One-click navigation inside a section (Forecasting, Aggregate Planning).
# Replaces the navbar drop-down menus with a row of pill buttons that stays at
# the top of the screen while scrolling. Group labels separate the methods, and
# Previous / Next buttons step through the pages in order.
#
# Usage mirrors navbarMenu(): pass tabPanel()s, and a character string before a
# tabPanel to start a new group, e.g.
#   section_tabs(ns("page"), tabPanel("Data", ...), "Stationary", tabPanel("MA", ...))

section_tabs <- function(id, ...) {
  items <- list(...)
  is_label <- vapply(items, is.character, logical(1))
  panels <- items[!is_label]

  # Group label -> value of the first page in that group
  labels <- list()
  for (i in which(is_label)) {
    nxt <- which(!is_label & seq_along(items) > i)
    if (length(nxt) > 0) {
      labels[[length(labels) + 1]] <- list(text = items[[i]],
                                           value = items[[nxt[1]]]$attribs$`data-value`)
    }
  }

  tabs <- do.call(tabsetPanel, c(list(id = id, type = "pills"), panels))
  q <- htmltools::tagQuery(tabs)
  for (lab in labels) {
    q$find("li")$filter(function(x, i) {
      a <- if (length(x$children) > 0) x$children[[1]] else NULL
      inherits(a, "shiny.tag") && identical(a$attribs$`data-value`, lab$value)
    })$before(tags$li(class = "section-nav-label", lab$text))
    q$resetSelected()
  }
  q$find("ul")$addClass("section-nav")$append(
    tags$li(class = "section-nav-step",
            tags$button(type = "button", class = "btn btn-default btn-sm", `data-step` = "-1",
                        title = "Previous page (Alt + Left arrow)", HTML("&larr; Previous")),
            tags$button(type = "button", class = "btn btn-default btn-sm", `data-step` = "1",
                        title = "Next page (Alt + Right arrow)", HTML("Next &rarr;")))
  )
  q$allTags()
}

# CSS and JavaScript for section_tabs(); include once in the page
section_nav_assets <- function() {
  tagList(
    tags$style(HTML("
      .section-nav {
        position: sticky; top: 0; z-index: 900;
        background: #fff; padding: 6px 0; margin-bottom: 14px;
        border-bottom: 1px solid #ddd;
        display: flex; flex-wrap: wrap; align-items: center; row-gap: 4px;
      }
      .section-nav > li { float: none; }
      .section-nav > li > a { padding: 5px 11px; }
      .section-nav > li.section-nav-label {
        font-size: 11px; font-weight: bold; text-transform: uppercase;
        color: #888; padding: 0 4px 0 14px; letter-spacing: .03em;
      }
      .section-nav > li.section-nav-label:first-child { padding-left: 4px; }
      .section-nav > li.section-nav-step { margin-left: auto; padding-left: 14px; }
      .section-nav > li.section-nav-step .btn { margin-left: 4px; }
    ")),
    tags$script(HTML("
      (function() {
        function pageLinks(nav) {
          return $(nav).find('> li > a[data-toggle=\"tab\"], > li > a[data-bs-toggle=\"tab\"]');
        }
        function step(nav, dir) {
          var links = pageLinks(nav);
          var cur = links.index(links.filter(function() {
            return $(this).parent().hasClass('active') || $(this).hasClass('active');
          }));
          var next = Math.min(Math.max(cur + dir, 0), links.length - 1);
          if (next !== cur) {
            links.eq(next).tab('show');
            window.scrollTo({ top: 0 });
          }
        }
        $(document).on('click', '.section-nav .section-nav-step .btn', function() {
          step($(this).closest('.section-nav'), parseInt($(this).data('step'), 10));
        });
        // Alt + Left / Right arrow: previous / next page of the visible section
        $(document).on('keydown', function(e) {
          if (!e.altKey || (e.key !== 'ArrowLeft' && e.key !== 'ArrowRight')) return;
          var nav = $('.section-nav:visible').first();
          if (!nav.length) return;
          e.preventDefault();
          step(nav, e.key === 'ArrowRight' ? 1 : -1);
        });
      })();
    "))
  )
}
