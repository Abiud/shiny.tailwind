#' Wrapper around [`shiny::selectInput()`] but allowing for more classes
#'
#' @inheritParams shiny::selectInput
#' @param container_class additional classes to be applied to the container
#' @param label_class additional classes to be applied to the label
#' @param select_class additional classes to be applied to the select elements
#'
#' @seealso [shiny::selectInput()]
#'
#' @return a list with a `shiny.tag` class
#'
#' @export
#' @examples
#' # Basic example
#' shiny::selectInput("id", "label", c("A" = "a", "B" = "b", "C" = "c"),
#'   selected = c("a", "b"), width = "200px",
#'   multiple = TRUE
#' )
#' twSelectInput("id", "label", c("A" = "a", "B" = "b", "C" = "c"),
#'   selected = c("a", "b"), width = "200px",
#'   multiple = TRUE, selectize = TRUE,
#'   container_class = "CONTAINER", label_class = "LABEL",
#'   select_class = "SELECT"
#' )
#'
#' # Grouped choices example
#' twSelectInput("id_grouped", "Grouped Label",
#'   choices = list(
#'     "Group A" = list("A1" = "a1", "A2" = "a2"),
#'     "Group B" = c("B1" = "b1", "B2" = "b2"),
#'     "C" = "c1" # Can mix groups and individual items
#'   ),
#'   selected = "b1",
#'   select_class = "text-blue-700"
#' )
#'
#' # basic full shiny example
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     use_tailwind(),
#'     twSelectInput(
#'       "variable", "Variable to select:",
#'       c("Cylinders" = "cyl", "Transmission" = "am", "Gears" = "gear"),
#'       multiple = TRUE,
#'       # Apply tailwind classes
#'       container_class = "shadow-md rounded-md bg-gray-50 m-4 p-2 w-72",
#'       label_class = "font-serif",
#'       select_class = "font-mono font-bold text-red-800 rounded-md bg-stone-50"
#'     ),
#'     tableOutput("data")
#'   )
#'
#'   server <- function(input, output) {
#'     output$data <- renderTable(
#'       {
#'         # Ensure input$variable is not NULL before accessing mtcars
#'         req(input$variable)
#'         mtcars[, c("mpg", input$variable), drop = FALSE]
#'       },
#'       rownames = TRUE
#'     )
#'   }
#'   shinyApp(ui, server)
#' }
twSelectInput <- function(
  inputId,
  label,
  choices,
  selected = NULL,
  multiple = FALSE,
  selectize = TRUE,
  width = NULL,
  size = NULL,
  container_class = NULL,
  label_class = NULL,
  select_class = NULL
) {
  # Argument validation
  if (selectize && !is.null(size)) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }

  # Resolve choices and selected value(s) using shiny's internal logic
  # This handles named/unnamed vectors/lists and detects grouped lists.
  choices <- shiny:::choicesWithNames(choices)

  # Determine default selection if necessary
  if (is.null(selected)) {
    if (!multiple) {
      # Find the first actual choice value, respecting groups
      first_val <- NULL
      if (length(choices) > 0) {
        first_element <- choices[[1]]
        # Check if the first element represents a group (is itself a list/vector)
        if (
          is.list(first_element) ||
            (is.atomic(first_element) &&
              !is.null(names(first_element)) &&
              length(first_element) > 1)
        ) {
          if (length(first_element) > 0) {
            first_val <- first_element[[1]] # Value of first item in first group
          }
        } else {
          # Ungrouped list/vector or single item group, get the first choice value
          first_val <- first_element
        }
        # This default selection logic might need refinement for edge cases like empty groups.
      }
      selected <- first_val
    } else {
      selected <- character(0) # Default for multiple is empty selection
    }
  }

  # CSS classes and width
  container_class <- paste(
    "block twSelectInput form-group",
    container_class
  )
  label_class <- paste("control-label", label_class)
  select_class <- paste("block form-control", select_class) # form-control might be bootstrap-specific, adjust if needed
  width <- shiny::validateCssUnit(width)

  # Helper function to generate <option> tags
  render_option <- function(value, name, is_selected) {
    shiny::tags$option(
      value = value,
      selected = if (is_selected) "selected" else NULL,
      name
    )
  }

  # Generate the select input options, handling groups using shiny's logic
  # shiny:::selectOptions() correctly handles grouped and non-grouped choices
  select_options <- shiny:::selectOptions(choices, selected)

  # Build the HTML structure
  label_id <- paste0(inputId, "-label")

  # Core select tag
  select_tag <- shiny::tags$select(
    id = inputId,
    class = select_class,
    multiple = if (multiple) "multiple" else NULL,
    size = if (!selectize && !is.null(size)) size else NULL,
    # Apply aria-labelledby for accessibility
    `aria-labelledby` = label_id,
    select_options # Add the generated <option> and <optgroup> tags
  )

  res <- shiny::div(
    class = container_class,
    style = if (!is.null(width)) paste0("width: ", width, ";") else NULL,
    if (!is.null(label))
      shiny::tags$label(
        # Conditionally render label
        class = label_class,
        id = label_id,
        `for` = inputId,
        label
      ),
    # If using selectize, it often wraps the select; otherwise, put select directly
    if (selectize) {
      shiny::div(
        # Selectize might need an extra wrapper, or maybe not. Test this.
        select_tag,
        shiny::tags$script(
          type = "application/json",
          `data-for` = inputId,
          `data-nonempty` = "",
          # Pass configuration for selectize if needed
          '{"plugins":["selectize-plugin-a11y"]}'
        )
      )
    } else {
      select_tag
    }
  )

  # Add selectize dependencies if needed
  if (selectize) {
    # Get dependencies from a dummy selectInput
    # Using a unique ID to avoid potential clashes
    dummy_id <- paste0(
      "dummy-selectize-dep-",
      paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
    )
    dummy_select <- shiny::selectInput(
      dummy_id,
      label = NULL,
      choices = "a",
      selectize = TRUE
    )
    attr(res, "html_dependencies") <- attr(dummy_select, "html_dependencies")
  }

  res
}
