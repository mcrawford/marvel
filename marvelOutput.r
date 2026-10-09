# Print BGG forum markup, or write it directly to a copy-and-paste text file.
MarvelOutput <- function(marvelGlm, file = "") {
  # Each group stays intact in one column. Choose the closest balanced split
  # while preserving the existing release order (down the left, then right).
  columns <- function(groups) {
    if (length(groups) < 2L) return(groups)
    heights <- vapply(groups, length, integer(1))
    split <- which.min(abs(2 * cumsum(heights)[-length(heights)] - sum(heights)))
    list(unlist(groups[seq_len(split)], use.names = FALSE),
         unlist(groups[seq.int(split + 1L, length(groups))], use.names = FALSE))
  }

  tableBlock <- function(title, data, headings, nameWidth = 0L, showHeadings = TRUE) {
    cells <- cbind(rownames(data), as.matrix(data))
    widths <- vapply(seq_len(ncol(cells)), function(i) {
      max(nchar(c(headings[i], cells[, i]), type = "width"))
    }, integer(1))
    widths[1L] <- max(widths[1L], nameWidth)
    formatRow <- function(row) {
      padded <- vapply(seq_along(row), function(i) {
        padding <- strrep(" ", widths[i] - nchar(row[i], type = "width"))
        if (i == 1L) paste0(row[i], padding) else paste0(padding, row[i])
      }, character(1))
      # Leave a little room between independent floated columns.
      paste0(paste(padded, collapse = "  "), "   ")
    }
    c(paste0("[b]", title, "[/b]"), "[c]",
      if (showHeadings) formatRow(headings),
      apply(cells, 1L, formatRow), "[/c]", "")
  }

  section <- function(title, groups, description = NULL) {
    blocks <- lapply(columns(groups), function(lines) {
      c("[floatleft]", lines, "[/floatleft]")
    })
    c(paste0("[b]", title, "[/b]"), description, "",
      unlist(blocks, use.names = FALSE), "[clear]", "")
  }

  scenarios <- MarvelFactors(marvelGlm)
  releases <- c(
    "Rhino" = "Core Set",
    "Wrecking Crew" = "Wrecking Crew",
    "Risky Business" = "Green Goblin",
    "Crossbones" = "Rise of Red Skull",
    "Kang" = "Kang",
    "Brotherhood of Badoon" = "Galaxy's Most Wanted",
    "Ebony Maw" = "Mad Titan's Shadow",
    "The Hood" = "The Hood",
    "Sandman" = "Sinister Motives",
    "Sabretooth" = "Mutant Genesis",
    "MaGog" = "Mojo Mania",
    "Morlock Siege" = "NeXt Evolution",
    "Unus" = "Age of Apocalypse",
    "Black Widow" = "Agents of S.H.I.E.L.D.",
    "Enchantress" = "Enchantress / God of Lies",
    "Captain America" = "Civil War",
    "Art Museum Heist" = "Fear No Evil"
  )
  starts <- match(names(releases), rownames(scenarios))
  stopifnot(!anyNA(starts), !is.unsorted(starts))
  ends <- c(starts[-1L] - 1L, nrow(scenarios))
  scenarioGroups <- lapply(seq_along(starts), function(i) {
    tableBlock(unname(releases[i]), scenarios[starts[i]:ends[i], , drop = FALSE],
               c("Scenario", "Multi", "Solo"), max(nchar(rownames(scenarios))),
               showHeadings = FALSE)
  })

  modifiers <- MarvelEncounterSets(marvelGlm)
  # Empty ratings mark release headings; the final blank heading introduces
  # difficulty options rather than another encounter-set release.
  starts <- which(modifiers$Ratings == "")
  ends <- c(starts[-1L] - 1L, nrow(modifiers))
  modifierGroups <- lapply(seq_along(starts), function(i) {
    title <- trimws(sub(" - $", "", sub("^ - ", "", rownames(modifiers)[starts[i]])))
    data <- modifiers[seq.int(starts[i] + 1L, ends[i]), , drop = FALSE]
    if (title == "") {
      title <- "Difficulty Options"
      rownames(data)[rownames(data) == "Heroic"] <- "Heroic (per level)"
      rownames(data)[rownames(data) == "Expert II"] <- "Expert II (add to Expert)"
    }
    tableBlock(title, data, c("Name", "Rating"),
               max(nchar(rownames(modifiers)[modifiers$Ratings != ""])),
               showHeadings = FALSE)
  })

  lines <- c(section("Scenarios", scenarioGroups,
                     "First number: multiplayer. Second number: solo (one hero)."),
             section("Encounter Sets and Adjustments", modifierGroups[-length(modifierGroups)]),
             modifierGroups[[length(modifierGroups)]])
  # Keep at most one blank line, including whitespace-only lines.
  blank <- !nzchar(trimws(lines))
  lines <- lines[!(blank & c(FALSE, head(blank, -1L)))]
  # Float tags already add spacing in BGG's renderer.
  beforeFloat <- c(tail(lines, -1L) %in% c("[floatleft]", "[/floatleft]"), FALSE)
  lines <- lines[!(!nzchar(trimws(lines)) & beforeFloat)]
  markup <- paste0(paste(lines, collapse = "\n"), "\n")
  cat(markup, file = file)
  invisible(markup)
}
