grouped_reactable <- function(data, 
                              group_var = NULL, 
                              id = "1", 
                              height = 500,
                              sortable = TRUE,
                              searchable = TRUE,
                              filterable = TRUE,
                              resizable = TRUE,
                              download = TRUE,
                              ...) {
  id <- paste0("expandable-table-", id)
  
  if (is.null(group_var)) {
    
    htmltools::browsable(
      if (download) {
        htmltools::tagList(
          download_csv_button(id),
          
          reactable::reactable(
            data,
            groupBy = group_var,
            sortable = sortable,
            searchable = searchable,
            filterable = filterable,
            resizable = resizable,
            defaultPageSize = 5,
            height = height,
            showPageSizeOptions = TRUE,
            elementId = id,
            ...
          )
        )
      } else {
        htmltools::tagList(
          reactable::reactable(
            data,
            groupBy = group_var,
            sortable = sortable,
            searchable = searchable,
            filterable = filterable,
            resizable = resizable,
            defaultPageSize = 5,
            height = height,
            showPageSizeOptions = TRUE,
            elementId = id,
            ...
          )
        )
      }
    )
  } else {
    htmltools::browsable(
      if (download) {
        htmltools::tagList(
          expand_toggle_button(id),
          download_csv_button(id),
          
          reactable::reactable(
            data,
            groupBy = group_var,
            sortable = TRUE,
            searchable = TRUE,
            filterable = TRUE,
            resizable = TRUE,
            defaultPageSize = 5,
            height = height,
            showPageSizeOptions = TRUE,
            elementId = id,
            ...
          )
        )
      } else {
        htmltools::tagList(
          expand_toggle_button(id),
          
          reactable::reactable(
            data,
            groupBy = group_var,
            sortable = TRUE,
            searchable = TRUE,
            filterable = TRUE,
            resizable = TRUE,
            defaultPageSize = 5,
            height = height,
            showPageSizeOptions = TRUE,
            elementId = id,
            ...
          )
        )
      }
    )
  }
}

expand_toggle_button <- function(table_id, label = "Expand/collapse all") {
  htmltools::tags$button(
    label,
    onclick = sprintf("Reactable.toggleAllRowsExpanded('%s')", table_id)
  )
}

download_csv_button <- function(table_id, label = "Download as CSV") {
  htmltools::tags$button(
    label,
    onclick = sprintf("Reactable.downloadDataCSV('%s')", table_id)
  )
}
