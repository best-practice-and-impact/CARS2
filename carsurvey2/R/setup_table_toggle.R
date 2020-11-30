#'@title Set up JavaScript functions for show table/chart buttons 
#'
#'@description Insert JavaScript code for show table/chart buttons. Use with insert_table_toggle().
#'Only include once per document. Assumes the naving convention "name-table" and "name-chart" for divs containing tables and charts.
#'
#'@return JavaScript code (raw html)
#'
#'@export

setup_table_toggle <- function() {
  
  script <- 
'
  <script>
    function show_table(output_name) {
      $("#show-table-" + output_name).hide();
      $("#show-chart-" + output_name).show();
      $("#" + output_name + "-chart").hide();
      $("#" + output_name + "-table").show();
    }
  
  function show_chart(output_name) {
    $("#show-table-" + output_name).show();
    $("#show-chart-" + output_name).hide();
    $("#" + output_name + "-chart").show();
    $("#" + output_name + "-table").hide();
  }
  </script>
'
  
  knitr::raw_html(script)
  
}