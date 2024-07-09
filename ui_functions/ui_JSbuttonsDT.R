rowNames <- TRUE # whether to show row names in the table
colIndex <- as.integer(rowNames)
callback <- c(
  sprintf("table.on('click', 'td:nth-child(%d)', function(){", colIndex+1),
  "  var td = this;",
  "  var cell = table.cell(td);",
  "  if(cell.data() === 'triangle-right'){",
  "    cell.data('triangle-left');",
  "  } else {",
  "    cell.data('triangle-right');",
  "  }",
  "  var $row = $(td).closest('tr');",
  "  $row.toggleClass('included');",
  "  var includedRows = [];",
  "  table.$('tr').each(function(i, row){",
  "    if($(this).hasClass('included')){",
  "      includedRows.push(parseInt($(row).attr('id').split('_')[1]));",
  "    }",
  "  });",
  "  Shiny.setInputValue('excludedRows', includedRows);",
  "})"
) 
render <- c(
  'function(data, type, row, meta){',
  '  if(type === "display"){',
  '    var color = data === "triangle-left"? "#edf794" :"black" ;',
  '    return "<span style=\\\"color:" + color +',
  '           "; font-size:18px\\\"><i class=\\\"glyphicon glyphicon-" +', 
  '           data + "\\\"></i></span>";',
  '  } else {',
  '    return data;',
  '  }',
  '}'
)