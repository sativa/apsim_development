//////////////////////////////////////////////////////////////////
// GridEX JavaScript IE 5.X UI (1.1.1009)
// Copyright by Janus Systems S.A.
// 2002 - 2004
//////////////////////////////////////////////////////////////////
var dragcolumn = null; 
var resizeline = null;  
var canceledByUser = false; 
var columnResizing = false; 
var columnDraging = false; 
var couldDragColumn = null;
var couldDragHeader = null;
var couldResizeColumn = null;  
var couldResizeColumnSet = null; 
var couldResizeHeader = null; 
var couldStartDrag = false;
var couldStartResize = false; 
var dragpoint = null; 
var resizepoint = null; 
var columnresizeMode = -1; 
var columndragMode = -1;
var currpressedcolumn = null; 
var currcolumn = null; 
var currgroupbybox = null; 
var groupnewpos = null; 
var currgrouppos = null;
var currgrouptable = null; 
var currcolumnset = null; 
var currheader = null;
var columnfordrop = null;
var droptarget = -1;
var divtt = null;
function body_onmousemove()
{
	if(couldStartDrag && dragpoint != null)
	{		
		if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
		{	
			startColumnDrag(couldDragColumn, couldDragHeader);
			couldStartDrag = false;
			couldDragColumn = couldDragHeader = null; 
			dragpoint = null; 
		}			
		window.event.cancelBubble = true; 
	}
	else if(columnDraging)
	{
		showColumnDrag();
		window.event.cancelBubble = true; 
	}
}
function gridEX_onmousemove()
{
	if(columnResizing)
		showResizeLine();
	else if(columnDraging)
		showColumnDrag();
}
function gridEX_onmouseup()
{
	if(window.event.button == 1)
	{
		if(columnResizing)
		{
			if(columnresizeMode == 1)
				endColumnResize();
			else if(columnresizeMode == 2)
				endColumnSetResize(); 					
		}
		else if(columnDraging)
			cancelColumnDraging();
	}	
}
function gridEX_onselectstart()
{	
	if(window.event.srcElement != null)
	{
		var element = window.event.srcElement;
		if(element.tagName != null)
		{
			if(element.tagName == "INPUT" && element.type != null && (element.type == "text" || element.type == "password"))
					return;
				if(element.tagName == "TEXTAREA")
					return;		
		}
	}
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return false;
}
function header_onselectstart()
{		
	window.event.cancelBubble = true;
	window.event.returnValue = false; 
	return true;
}
function header_onmousedown()
{
	var element = window.event.srcElement;
}
function hcscolumn_oncontextmenu()
{
	window.event.cancelBubble = true; 
	window.event.returnValue = false; 
}
function hcscolumn_onselectstart()
{
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return true; 
}
function hcscolumn_onmousemove()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("columnset").column_onmousemove();
}
function hcscolumn_onmouseover()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("columnset").column_onmouseover();
}
function hcscolumn_onmousedown()
{	
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("columnset").column_onmousedown();
}
function hcscolumn_onmouseup()
{	
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("columnset").column_onmouseup();
}
function hcscolumn_ondblclick()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("columnset").column_ondblclick();
}
function hcscolumn_onclick()
{
	var column = getColumnFromElement(window.event.srcElement); 
	column.getAttribute("columnset").column_onclick(); 
}
function table_onmousemove()
{
	if(couldStartDrag && dragpoint != null)
	{		
		if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
		{	
			startColumnDrag(couldDragColumn, couldDragHeader);
			couldStartDrag = false;
			couldDragColumn = couldDragHeader = null; 
			dragpoint = null; 				
			window.event.cancelBubble = true; 
		}
	}
	else if(columnDraging)
	{
		showColumnDrag(); 
		window.event.cancelBubble = true; 
	}					
}
function table_onselectstart()
{
	if(window.event.srcElement != null)
	{
		var element = window.event.srcElement;
		if(element.tagName != null)
		{
			if(element.tagName == "INPUT" && element.type != null && (element.type == "text" || element.type == "password"))
					return;
				if(element.tagName == "TEXTAREA")
					return;		
		}		
	}
	window.event.returnValue = false;
	window.event.cancelBubble = true;
	return true; 
}
function gtable_onblur()
{
	var element = window.event.srcElement;
	if(element.getAttribute("table") != null)	
		element.getAttribute("table").table_onblur();		
}
function gtable_onscroll()
{	
	var element = window.event.srcElement;
	if(element.getAttribute("table") != null)		
		element.getAttribute("table").table_onscroll();
}
function gcolumnset_onselectstart()
{		
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return true; 
}	
function gbbcolumn_onmousedown()
{
	var column = getColumnFromElement(window.event.srcElement); 
	column.getAttribute("groupbybox").column_onmousedown();
}
function gbbcolumn_onmouseup()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("groupbybox").column_onmouseup();
}
function gbbcolumn_onmousemove()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("groupbybox").column_onmousemove();
}
function hcolumn_onmousedown()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("header").column_onmousedown();
}
function hcolumn_onmousemove()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("header").column_onmousemove();
}
function hcolumn_onmouseover()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("header").column_onmouseover();
}
function hcolumn_onmouseup()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("header").column_onmouseup();
}
function hcolumn_onclick()
{
	var column = getColumnFromElement(window.event.srcElement); 
	column.getAttribute("header").column_onclick(); 
}
function hcolumn_ondblclick()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("header").column_ondblclick();
}
function hcolumn_oncontextmenu()
{
	var column = getColumnFromElement(window.event.srcElement);
	column.getAttribute("header").column_oncontextmenu();
}
function ggridEX_onkeydown(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onkeydown();
}
function ggridEX_onload(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onload(); 
}
function ggridEX_onsubmit(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onsubmit();
}
function ggridEX_onunload(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onunload();
}
function ggridEX_onresize(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onresize();
}
function ggridEX_onblur(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onblur();
}
function gbody_onselectstart(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.body_onselectstart();
}
function ggridEX_onkeydown(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onkeydown();
}
function ggridEX_onmousewheel(id)
{
	var gridex = getGridEXFromID(id);	
	gridex.gridEX_onmousewheel();
}
function Point(x, y)
{
	var x = x;
	var y = y; 	
	this.X = X;
	this.Y = Y;
	function X() { return x; }	
	function Y() { return y;  }
}
function PasswordText(text,passwordChar)
{
	var mask = "";
	for(var i=0;i<text.length;i++)
		mask += passwordChar;
	return mask; 
}
function trim(text)
{        	
	if(text == null)
		return ""; 
	var charCode = -1; 	
	do
	{
		charCode = text.substring(0,1).charCodeAt();
		if(charCode == 32 || charCode == 160)
			text = text.substring(1, text.length);			
	} while(charCode == 32 || charCode == 160); 
    do
    {
		charCode = text.substring(text.length-1,text.length); 
		if(charCode == 32 || charCode == 160)
			text = text.substring(0, text.length-1); 			
    } while(charCode == 32 || charCode == 160);    
   return text;
}
function previewrow_oncollapse(cell, gridexID, tableID)
{
	if(cell.nodeType == 1 && cell.tagName == "TD")
	{
		var _gridex = getGridEXFromID(gridexID); 
		if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
		{
			var _gridexTable = _gridex.getTables().getTableByID(tableID); 			
			var _row = cell.parentElement; 						
			var _gridexRow = _gridex.RetrieveRow(getRootRowFromInner(_row), _row, _gridexTable); 
			_gridexRow.CollapsePreviewRow(cell); 
		}
	}
}
function browseURLByRow(row)
{
	var _url = row.getURL();
	var _target = row.getURLTarget();
	if(_target == null || _target.length == 0)
		_target = "_self";
	
	window.open(_url, _target); 
}
function onSelectRow(innerRow, gridex, gridexTable)
{		
	var row = gridex.RetrieveRow(getRootRowFromInner(innerRow), innerRow, gridexTable);		
	gridex.setCurrentRow(row); 
	gridex.getSelectedItems().SelectRow(row); 	
	setRowForEditOrFilter(row);
}
function setRowForEditOrFilter(row)
{
	if(row.containsURL())
	{			
		browseURLByRow(row); 
		return; 
	}
	if(row.getType() == 9 || (row.getTable().getAllowEdit() && (row.getType() == 3 || row.getType() == 4)))
		row.BeforeEdit(); 
	else if(row.getType() == 11 && row.getGridEX().getFilterMode() == 1)
		row.BeforeFilter(); 
	else if(!row.getTable().getAllowEdit() && (row.getType() == 3 || row.getType() == 4))
	{
		var cell = row.getCellSelected();
		if(cell != null)
		{								
			if(cell.getColumn().getSelectable() && cell.getColumn().getActAsSelector())
				row.CheckRow(cell.getValue(),cell.getColumn().getClientID(),true,true);
			else if(cell.getColumn().getColumnType() == 4)
			{				
				window.event.cancelBubble = true;
				window.event.returnValue = false;
				return false;
			}				
		}
	}
}
function getInnerItemRowCore(row, gridex)
{
	var _innerItemRow = null; 
	var _tableID = null; 
	if(gridex.isHierarchicalGrid())
	{			
		var _tmpTable = row.cells[0].childNodes[0]; 
		var _tmpRow = _tmpTable.rows[0]; 
		var l = _tmpRow.cells.length; 
		for(var i = 0; i < l && _innerItemRow == null; i++)
		{
			if(_tmpRow.cells[i].childNodes[0].tagName == "TABLE")
				_innerItemRow = _tmpRow.cells[i].childNodes[0].rows[0]; 
		}			
		if(_innerItemRow == null)			
			throw Error("unable to retrieve inner row"); 
	}		
	if(row.getAttribute("t") != null)
		_tableID = row.getAttribute("t"); 
		
	if(_tableID != null && gridex.getTables().getTableByID(_tableID).getUseColumnSets())
	{
		if(_innerItemRow == null)
			_innerItemRow = row;
			
		if(getType() == 3 || getType() == 9)				
			return _innerItemRow.cells[0].childNodes[0].rows[0]; 
		else
			return _innerItemRow; 
	}
	else if(gridex.isHierarchicalGrid() && _innerItemRow != null)
		return _innerItemRow;
	else
		return row;
}
function selectionChanged(row, rowID, tableID, gridexID)
{
	var _row = null; 
	var _gridex = getGridEXFromID(gridexID);
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		_row = _gridex.RetrieveRow(getRootRowFromInner(row), row, _gridex.getTables().getTableByID(tableID))
		if(_gridex.getSelectedItems() != null && _gridex.getSelectedItems().Count() == 1)
		{
			if(_gridex.getSelectedItems().getSelectedItemInIndex(0).getRow() == _row)
			{
				setRowForEditOrFilter(_row);
				return false; 
			}
		}				
	}
	if(_row != null)
	{
		_gridex.setHitTestArea(0); 
		_gridex.FireEvent("Click", [_gridex, window.event.button, window.event.clientX, window.event.clientY]); 
		_gridex.FireEvent("SelectionChanged", [_row]); 
		_gridex.DoPostBack(null, "SelectionChanged:"+_row.getID());
	}
}
function row_onclick(row, tableID, gridexID)
{	
	var _gridex = getGridEXFromID(gridexID);
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		var _gridextable = _gridex.getTables().getTableByID(tableID);			
		onSelectRow(row, _gridex, _gridextable); 
		_gridex.setHitTestArea(0); 
		_gridex.FireEvent("Click", [_gridex,window.event.button, window.event.clientX, window.event.clientY]); 
		if(window.event.type == "contextmenu")
		{
			window.event.cancelBubble = true;
			window.event.returnValue = false;
		}
	}	
}
function row_ondblclick(row, tableID, gridexID)
{
	var _gridex = getGridEXFromID(gridexID);
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		var _gridextable = _gridex.getTables().getTableByID(tableID);			
		onSelectRow(row, _gridex, _gridextable); 
		_gridex.setHitTestArea(0); 
		_gridex.FireEvent("DoubleClick", [_gridex, window.event.clientX, window.event.clientY]); 
	}
}
function clickRowPreviewCore(_gridex, previewRow, tableID)
{
	var _gridextable = _gridex.getTables().getTableByID(tableID);			
	var row = null; 
	if(_gridex.isHierarchicalGrid())
	{
		row = previewRow.offsetParent.rows[previewRow.rowIndex - 1]; 
		if(_gridextable.getUseColumnSets())
			row = row.cells[0].childNodes[0].rows[0];
	}
	else
	{
		row = _gridextable.getHtmlItemsTable().rows[previewRow.rowIndex - 1]; 		
		if(_gridextable.getUseColumnSets())
			row = row.cells[0].childNodes[0].rows[0];
	}		
	onSelectRow(row, _gridex, _gridextable); 
	if(getTypeOfTD(window.event.srcElement) == "rh")
		_gridex.setHitTestArea(4);
	else
		_gridex.setHitTestArea(14); 
}
function rowpreview_onclick(previewRow, tableID, gridexID)
{	
	var _gridex = getGridEXFromID(gridexID); 
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		clickRowPreviewCore(_gridex, previewRow, tableID); 		
		_gridex.FireEvent("Click", [_gridex,window.event.button, window.event.clientX, window.event.clientY]); 		
		if(window.event.type == "contextmenu")
		{
			window.event.cancelBubble = true;
			window.event.returnValue = false; 			
		}
	}
}
function rowpreview_ondblclick(previewRow, tableID, gridexID)
{
	var _gridex = getGridEXFromID(gridexID); 
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		clickRowPreviewCore(_gridex, previewRow, tableID);
		_gridex.FireEvent("DoubleClick", [_gridex,window.event.clientX, window.event.clientY]);
	}
}
function row_oncollapse(rowID, tableID, gridexID, action)
{
	var _rootRow = document.getElementById(rowID); 
	if(_rootRow == null)
		throw Error("unable to find HTML TR for row id '" + rowID + "'"); 

	var _gridex = getGridEXFromID(gridexID); 
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		var _gridextable = _gridex.getTables().getTableByID(tableID);
		var _row = _gridex.RetrieveRow(_rootRow, null, _gridextable, null);
		if(action == 1)
			_row.Expanding();
		else if(action == 0)
			_row.Collapsing(); 	
	}
}
function loadedRow_oncollapse(rowID, tableID, gridexID)
{
	var _rootRow = document.getElementById(rowID); 
	if(_rootRow == null)
		throw Error("unable to find HTML TR for row id '" + rowID + "'"); 
		
	var _gridex = getGridEXFromID(gridexID); 
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		var _gridextable = _gridex.getTables().getTableByID(tableID);
		var _row = _gridex.RetrieveRow(_rootRow, null, _gridextable, null); 					
		if(_row.getExpanded())
			_row.Collapsing();
		else
			_row.Expanding();
		if(_row == _gridex.getGridEXRow() || !_gridex.getSelectOnExpand())
			_gridex.ReportRowsStatus();
		if(window.event.srcElement != null && _gridextable.getAutoSizeExpandColumn() != null)
		{
			var _td = getColumnFromElement(window.event.srcElement);
			if(_td != null && _td.getAttribute("type") == "ec" && _td.getAttribute("id") == _gridextable.getAutoSizeExpandColumn() + "_L")
				_gridextable.AutoSizeExpandColumn(_td); 
		}		
		_gridex.setCurrentRow(_row, _gridex.getSelectOnExpand());
		if(!_gridex.getSelectOnExpand())
		{
			window.event.cancelBubble = true;
			window.event.returnValue = false;
			return false;
		}
		return true; 
	}
}
function selector_checkItems(checkbox, columnID, tableID, gridexID, fireEvent)
{	
	var _gridex = getGridEXFromID(gridexID); 
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{				
		var _table = _gridex.getTables().getTableByID(tableID);
		var _headers = _table.getHeaders(); 
		if(_headers.length > 1)
		{
			for(var i = 0; i < _headers.length; i++)						
				_headers[i].CheckSelectors(columnID, checkbox.checked, true);
		}
		else if(_headers.length == 1)
			_headers[0].CheckSelectors(columnID, checkbox.checked, true); 
		else
			_headers.CheckSelectors(columnID, checkbox.checked, true);
			
		var _row = null; 
		var _rowType = -1; 
		var _innerRow = null;
		var _itemsTable = _gridex.getRootTable().getHtmlItemsTable(); 
		var _indexPos = 0; 
		var l = _itemsTable.rows.length; 
		for(var i=0;i<l;i++)
		{
			_innerRow = _itemsTable.rows[i];			
			if(_innerRow.getAttribute("t") == tableID) 
			{			
				_rowType = _innerRow.getAttribute("type"); 
				if(_rowType != "1" && _rowType != "2" && _rowType != "6" && _rowType != "7" && _rowType != "10")
				{				
					if(_rowType == "3" || _rowType == "4" || _rowType == null)
					{
						if(_innerRow.getAttribute("ri") != null)
							_row = _gridex.getRowInIndex(parseInt(_innerRow.getAttribute("ri"), 10)); 
						else
						{	
							var position = _indexPos;
							if(_gridex.getRootTable().getNewRowPosition() == 2) 
								position++;
							if(_gridex.getFilterMode() == 1)
								position++;
							_row = _gridex.RetrieveRow(_innerRow, null, _table, position, false); 
						}
						_row.CheckRow(checkbox.checked, null, null, (fireEvent != null && fireEvent == 1) ? true : false); 
					}
					_indexPos++; 	
				}
			}
		}		
		_gridex.ReportRowsStatus(); 
		_gridex.FireEvent("RowCheckedChanged", [checkbox.checked, _table, null]);
		if(fireEvent != null && fireEvent == 1)
		{
			var argument = "RowCheckedChanged";
			if(checkbox.checked)
				argument += ":3";
			else
				argument += ":4";
			_gridex.DoPostBack(null, argument); 
		}
	}
}
function drawDownArrow(x, y)
{	
	var index = 0;
	while(index < 5)
	{
		var line = document.all("da" + (index+1));
		if(line !=null)
		{
			line.style.pixelLeft = x -  index;
			line.style.pixelTop = y + index; 
			line.style.display = "";
			line.style.visibility = "visible"; 			
			index++;
		}
	}			
	var arrow = document.all("da" + (index+1));
	if(arrow != null)
	{	
		arrow.style.pixelLeft = x - 1;
		arrow.style.pixelTop = y + index; 
		arrow.style.display = "";
		arrow.style.visibility = "visible"; 
	}
}	
function drawUpArrow(x, y)
{		
	var index = 0;
	var line = null; 
	var _style = null; 
	while(index < 5)
	{	
		line = document.getElementById("ua" + (index+1));
		if(line != null)
		{
			_style = line.style; 		
			_style.pixelLeft = x - index;
			_style.pixelTop = y - index;
			_style.display = "";
			_style.visibility = "visible";
			index++; 			
		}
	}	
	var arrow = document.getElementById("ua" + (index+1));
	if(arrow != null)
	{
		_style = arrow.style;
		_style.pixelLeft = x - 1;
		_style.pixelTop = y - 4 - _style.pixelHeight;
		_style.display = "";
		_style.visibility = "visible"; 
	}
}
function hideColumnForDrop()
{	
	for(var index = 1; index <= 6; index++)
	{
		var _a = document.getElementById("da" + index);
		_a.style.display = "none"; 
		_a.style.visibility = "hidden";
		var _b = document.getElementById("ua" + index);
		_b.style.display = "none";
		_b.style.visibility = "hidden"; 
	}
}
function drag_onselectstart()
{
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return false; 
}
function drag_onmousemove()
{
	if(columnDraging && currcolumn != null)	
	{
		var _result = null; 	
		showColumnDrag();		
		if(columndragMode == 1)
		{	
			droptarget = -1; 
			_result = currheader.getGridEXTable().HitTestColumnHeaders(window.event.clientX, window.event.clientY);			
			if(_result != null && _result[0] != null && _result[0].id != currcolumn.id)
			{
				droptarget = 1;
				_result[1].ShowColumnForDrop(window.event.clientX, _result[0]); 
				currheader = _result[1]; 
			}
			else if(currheader.getGridEX().getGroupByBox() != null)
			{	
				if(currheader.getGridEXTable().getColumns().getGridEXColumnByClientID(currcolumn.id).getAllowGroup())
				{
					_result = currheader.getGridEX().getGroupByBox().HitTestColumns(window.event.clientX, window.event.clientY, 1);
					if(_result != null && _result[0] != null)
					{
						droptarget = 2;
						currgroupbybox = currheader.getGridEX().getGroupByBox();
						currgroupbybox.ShowColumnForDrop(window.event.clientX, _result[0], _result[1], _result[2]); 
					}
				}
			}			
			if(_result == null)								
				hideColumnForDrop();				
		}
		else if(columndragMode == 2)
		{
			droptarget = -1;
			if(currheader.getGridEX().getGroupByBox() != null)
			{
				_result = currheader.getGridEX().getGroupByBox().HitTestColumns(window.event.clientX, window.event.clientY, 1); 
				if(_result != null && _result[0] != null)
				{
					droptarget = 2; 
					currgroupbybox = currheader.getGridEX().getGroupByBox();
					currgroupbybox.ShowColumnForDrop(window.event.clientX, _result[0], _result[1], _result[2]); 					
				}								
				if(_result == null)
					hideColumnForDrop(); 
			}			
		}
		else if(columndragMode == 3) 
		{			
			droptarget = -1;			
			_result = currgroupbybox.HitTestColumns(window.event.clientX, window.event.clientY, 2); 
			if(_result != null && _result[0] != null && _result[0].id != currcolumn.id)
			{				
				droptarget = 2;
				currgroupbybox.ShowColumnForDrop(window.event.clientX, _result[0], _result[1], 0); 
			}
			else
			{	
				var _tableindex = currgroupbybox.getGridEX().getTables().getIndexOf(currgrouptable); 
				if(_tableindex != -1)
				{
					var _table = currgroupbybox.getGridEX().getTables().getTableInIndex(_tableindex); 
					_result = _table.HitTestColumnHeaders(window.event.clientX, window.event.clientY); 
					if(_result != null && _result[0] != null)
					{
						droptarget = 1;
						currheader = _result[1];
						currheader.ShowColumnForDrop(window.event.clientX, _result[0]); 
					}
				}
			}					
			if(_result == null)
				hideColumnForDrop(); 
		}
	}
}
function drag_onmouseup()
{
	if(window.event.button == 1)
	{ 
		if(columnDraging && currcolumn != null) 
		{			
			if(droptarget == 1)
			{					
				currheader.DropColumn(currcolumn); 
				endColumnDrag();
				if(columndragMode != 3)
					ShowColumnUnPressed();
	
				currpressedcolumn = null; 
			}
			else if(droptarget == 2)
				currgroupbybox.DropColumn();
			else
			{
				if(columndragMode == 3 && currgroupbybox != null)
					currgroupbybox.DropColumn(); 
				else
				{				
					cancelColumnDraging();
					ShowColumnUnPressed();
					currpressedcolumn = null;
				}
			}
		}		
	}
}
function startGroupDrag(groupcolumn, gridEXGroupByBox, grouptable)
{
	canceledByUser = false; 
	columnDraging = true;
	currcolumn = groupcolumn; 
	currgroupbybox = gridEXGroupByBox;
	currgrouptable = grouptable; 
	columndragMode = 3; 
	showColumnDrag(); 
}
function  startColumnDrag(column, gridEXHeader)
{
	columnDraging = true; 
	currcolumn = column;
	currheader = gridEXHeader;
	columndragMode = 1; 
	showColumnDrag();
}
function startColumnSetDrag(column, gridEXHeader, gridEXColumnSet)
{
	canceledByUser = false; 
	columnDraging = true;
	currcolumn = column;
	currheader = gridEXHeader;	
	currcolumnset = gridEXColumnSet; 
	columndragMode = 2;
	showColumnDrag();
}
function endColumnDrag()
{
	columnDraging = false;
	currcolumn = null;
	currgroupbybox = null;	
	groupnewpos = null; 
	currgrouppos = null;
	currgrouptable = null;	
	currheader = null; 
	columnfordrop = null;	
	droptarget = -1; 
	hideColumnDrag(); 
}
function hideColumnDrag()
{
	dragcolumn.style.display = "none";
	dragcolumn.style.visibility = "hidden"; 
	hideColumnForDrop(); 
}
function showColumnHeaderDrag()
{			
	if(dragcolumn.style.visibility == "hidden")
	{		
		if(currcolumn == null)
			return; 
			
		var currstyle = currcolumn.currentStyle;		
		dragcolumn.style.backgroundColor = currstyle.backgroundColor; 
		dragcolumn.style.color = currstyle.color; 
		dragcolumn.style.backgroundImage = currstyle.backgroundImage; 
		var _colpressed = document.getElementById("colpressed"); 
		if(_colpressed != null)
		{						
			dragcolumn.style.borderBottom = _colpressed.style.borderBottom;
			dragcolumn.style.borderRight = _colpressed.style.borderRight;		
			dragcolumn.style.borderTop = _colpressed.style.borderTop;
			dragcolumn.style.borderLeft = _colpressed.style.borderLeft; 
		}
		else
		{	
			dragcolumn.style.borderBottom = currstyle.borderBottom; 
			dragcolumn.style.borderRight = currstyle.borderRight; 
			dragcolumn.style.borderTop = currstyle.borderTop; 
			dragcolumn.style.borderLeft = currstyle.borderLeft; 
		}		
		dragcolumn.style.padding = currstyle.padding;		
		if(currcolumn.font != null && currcolumn.font != "") 
			dragcolumn.style.font = currcolumn.font;
			
		dragcolumn.style.fontFamily = currstyle.fontFamily;
		dragcolumn.style.fontSize = currstyle.fontSize;
		dragcolumn.style.fontStyle = currstyle.fontStyle;
		dragcolumn.style.fontVariant = currstyle.fontVariant;
		dragcolumn.style.fontWeight = currstyle.fontWeight;
		dragcolumn.style.textAlign = currstyle.textAlign; 
	}	
	if(dragcolumn.style.visibility == "hidden")
	{				
		dragcolumn.style.pixelWidth =  currcolumn.offsetWidth; 
		dragcolumn.style.pixelHeight = currcolumn.offsetHeight; 
		dragcolumn.innerHTML =  currcolumn.childNodes[0].innerHTML; 
		dragcolumn.childNodes[0].style.padding = currcolumn.childNodes[0].currentStyle.padding; 		
		var _spanpressed = _colpressed.childNodes[0];
		var _child = dragcolumn.childNodes[0];
		_child.style.borderLeft = _spanpressed.style.borderLeft;
		_child.style.borderTop = _spanpressed.style.borderTop;
		_child.style.borderRight = _spanpressed.style.borderRight;
		_child.style.borderBottom = _spanpressed.style.borderBottom;
	}	
	if(dragcolumn.style.visibility == "hidden")
	{	
		if(columndragMode == 1) 
		{
			var _htmlheader = currheader.getHtmlHeader(); 			
			var divtable = currheader.getGridEXTable().getHtmlDiv(); 
			var _gridex = currheader.getGridEXTable().getGridEX();			
			var _isrtl = (_gridex.getHtmlGridEX().getAttribute("rtl") == "1");
			var _left = getPixelLeft(currcolumn);	
			if(!currheader.getIsRoot() && !_isrtl)
				_left -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
			else if(!currheader.getIsRoot())
			{
				_left += (currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
				_left += (currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}
			dragcolumn.style.pixelLeft = _left;			
			var _top = getPixelTop(currcolumn);
			if(!currheader.getIsRoot())
				_top -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollTop;
				
			var _offsety = (_top + currcolumn.offsetHeight) - dragpoint.Y();
			dragcolumn.style.pixelTop = (window.event.clientY + _offsety) - currcolumn.offsetHeight;  
			dragcolumn.varX = window.event.clientX - dragcolumn.style.pixelLeft; 
			dragcolumn.varY = window.event.clientY - dragcolumn.style.pixelTop;
		}
		else if(columndragMode == 2)	
		{
			var htmlTable = currcolumnset.getHtmlColumnSet(); 
			var divtable = currcolumnset.getGridEXTable().getHtmlDiv(); 
			var divgridex = currcolumnset.getGridEXTable().getGridEX().getHtmlGridEX();			
			var _left = getPixelLeft(currcolumn);
			var _isrtl = (divgridex.getAttribute("rtl") == "1");			
			dragcolumn.style.pixelLeft = _left; 			
			var _top = getPixelTop(currcolumn); 			
			var _offsety = (_top + currcolumn.offsetHeight) - dragpoint.Y(); 			
			dragcolumn.style.pixelTop = (window.event.clientY + _offsety) - currcolumn.offsetHeight; 						
			dragcolumn.varX = window.event.clientX - dragcolumn.style.pixelLeft;
			dragcolumn.varY = window.event.clientY - dragcolumn.style.pixelTop; 
		}
	}
	else
	{
		dragcolumn.style.pixelLeft = window.event.clientX - dragcolumn.varX;		
		dragcolumn.style.pixelTop = window.event.clientY - dragcolumn.varY; 
	}	
	dragcolumn.style.display = "";
	dragcolumn.style.visibility = "visible";
}
function showColumnGroupDrag()
{	
	if(dragcolumn != null)
	{
		var _colstyle = currcolumn.style;		
		dragcolumn.style.backgroundColor = _colstyle.backgroundColor;				
		dragcolumn.style.borderBottom = _colstyle.borderBottom; 
		dragcolumn.style.borderRight = _colstyle.borderRight; 
		dragcolumn.style.borderTop = _colstyle.borderTop; 
		dragcolumn.style.borderLeft = _colstyle.borderLeft;		
		dragcolumn.style.padding = _colstyle.padding;		
		if(currcolumn.style.font != null && currcolumn.style.font != "") 
			dragcolumn.style.font = currcolumn.style.font;
			
		dragcolumn.style.fontFamily = currcolumn.currentStyle.fontFamily;
		dragcolumn.style.fontSize = currcolumn.currentStyle.fontSize;
		dragcolumn.style.fontStyle = currcolumn.currentStyle.fontStyle;
		dragcolumn.style.fontVariant = currcolumn.currentStyle.fontVariant;
		dragcolumn.style.fontWeight = currcolumn.currentStyle.fontWeight;
		dragcolumn.style.textAlign = _colstyle.textAlign;		
	}	
	if(dragcolumn.style.visibility == "hidden")
	{
		dragcolumn.style.pixelWidth =  currcolumn.offsetWidth;  
		dragcolumn.style.pixelHeight = currcolumn.offsetHeight; 
		dragcolumn.innerHTML =  currcolumn.childNodes[0].outerHTML; 
		var _child = dragcolumn.childNodes[0];
		_child.style.borderLeft = currcolumn.childNodes[0].style.borderLeft;
		_child.style.borderTop = currcolumn.childNodes[0].style.borderTop;
		_child.style.borderRight = currcolumn.childNodes[0].style.borderRight;
		_child.style.borderBottom = currcolumn.childNodes[0].style.borderBottom;						
	}	
	if(dragcolumn.style.visibility == "hidden")
	{
		dragcolumn.style.pixelLeft = getPixelLeft(currcolumn); 
		dragcolumn.style.pixelTop = getPixelTop(currcolumn);
		dragcolumn.varX = window.event.clientX - dragcolumn.style.pixelLeft;
		dragcolumn.varY = window.event.clientY - dragcolumn.style.pixelTop; 
	}
	else
	{
		dragcolumn.style.pixelLeft = window.event.clientX - dragcolumn.varX;		
		dragcolumn.style.pixelTop = window.event.clientY - dragcolumn.varY; 
	}	
	dragcolumn.style.display = "";
	dragcolumn.style.visibility = "visible";
}
function showColumnDrag()
{	
	if(columndragMode == 3)
		showColumnGroupDrag(); 
	else
		showColumnHeaderDrag(); 	
}
function cancelColumnDraging()
{
	hideColumnDrag();	
	columnDraging = false; 
	currcolumn = null; 
	currheader = null; 
	currgrouptable = null;
	currgroupbybox = null; 	
}
function getMaximumColumnSize(gridEXColumn)
{
	var _cell = null; 
	var _cells = document.getElementsByName(gridEXColumn.getClientID() + "_L"); 
	var _cellsLength = _cells.length; 
	var _cellsScanned = 0;
	var _cellsToCount = 10; 
	var _cellWidth = -1; 
	var _realWidth = -1; 
	var _row = null; 	
	if(_cellsLength == 0)
		return -1;
		
	var _gridexid = gridEXColumn.getTable().getGridEX().getID(); 
	var _spancell = document.getElementById(_gridexid + "_spancell"); 	
	if(_spancell == null)
	{
		_spancell = document.createElement("SPAN");
		_spancell.id = _gridexid + "_spancell"; 
		_spancell.style.visibility = "hidden"; 
		document.body.appendChild(_spancell); 				
	}
	else
		_spancell.style.display = "";
	for(var i = 0; i < _cellsLength && _cellsScanned < _cellsToCount; i++)
	{		
		_cell = _cells[i]; 
		_row = getRootRowFromInner(_cell.parentElement); 
		if(_row.style.display != "none")
		{						
			_realWidth = getRealCellWidth(_spancell, _cell, _gridexid); 											
			if(_realWidth > _cellWidth)
				_cellWidth = _realWidth;
				
			_cellsScanned++; 
		}				
	}	
	if(_spancell != null)
	{
		_spancell.style.display = "none";
		_spancell.style.visibility = "hidden";		
	}	
	return _cellWidth; 
}
function drawResizeLine(left, top, height)
{	
	resizeline.style.pixelLeft = left;
	resizeline.style.pixelTop = top; 	
	resizeline.style.pixelHeight = height; 
	resizeline.style.display = "block";
	resizeline.style.visibility = "visible";
}
function resizeline_onmouseup()
{
	if(window.event.button == 1)
	{
		if(columnResizing)
		{
			if(columnresizeMode == 1)
				endColumnResize();
			else if(columnresizeMode == 2)
				endColumnSetResize(); 
		}
	}
}
function cancelColumnSetResize()
{
	hideColumnResizeLine();
	columnResizing = false;
	currcolumn = null;
	currcolumnset = null; 	
	couldStartResize = false;
	resizepoint = null; 
}
function startColumnSetResize(column, gridEXColumnSet)
{
	canceledByUser = false; 
	currcolumnset = gridEXColumnSet;
	currcolumn = column; 
	columnResizing = true;
	columnresizeMode = 2;
	showColumnSetResizeLine(); 	
}
function endColumnSetResize()
{	
	if(currcolumn != null && currcolumnset != null)
	{
		currcolumnset.ResizeColumnSetHeader(currcolumn, window.event.clientX); 	
		hideColumnResizeLine(); 
		columnResizing = false;
		currcolumn = null;
		currcolumnset = null;
	}
}
function showColumnSetResizeLine()
{	
	if(resizeline != null)	
	{			
		var _gridexTable = currcolumnset.getGridEXTable(); 
		var _gridex = _gridexTable.getGridEX();
		var _rtl = _gridex.getHtmlGridEX().getAttribute("rtl") == "1";
		var leftmin = -1;
		if(_rtl)
			leftmin = (getPixelLeft(currcolumn) + currcolumn.offsetWidth) - 9;
		else
			leftmin = getPixelLeft(currcolumn) - getHorizontalScrollOffset(_gridex) + 9; 
		if(currcolumn.type != null && currcolumn.type == "ch")
			leftmin += _gridexTable.getHeaderWidth(); 
			
		if(currcolumn.getAttribute("pec") != null && currcolumn.type != "ch")
			leftmin += 18; 
	
		if(_rtl)
		{
			if(window.event.clientX + getScrollLeft(_gridex) >= leftmin)
				return; 
		}
		else
		{
			if(window.event.clientX + getScrollLeft(_gridex) <= leftmin)
				return;
		}			
		var divgridex = _gridex.getHtmlGridEX(); 
		var divtable = currcolumnset.getGridEXTable().getHtmlDiv();				
		var left = window.event.clientX - 2;
		left += getScrollLeft(_gridex);
		var top = getPixelTop(currcolumn); 
		var height = _gridexTable.getHtmlItemsTable().offsetParent.offsetHeight;
		height -= (top - getPixelTop(_gridexTable.getHtmlItemsTable().offsetParent));		
		drawResizeLine(left, top, height); 
	}	
}
function startColumnResize(column, gridEXHeader)
{	
	canceledByUser = false; 
	currheader = gridEXHeader; 
	currcolumn = column; 
	columnResizing = true;
	columnresizeMode = 1;
	showColumnResizeLine();
}
function endColumnResize()
{	
	if(currcolumn != null && currheader != null)
	{			
		currheader.ResizeColumnWidth(currcolumn, event.clientX);
		hideColumnResizeLine();
		columnResizing = false;
		currcolumn = null;
		currheader = null; 		
	}
}
function cancelColumnResize()
{	
	hideColumnResizeLine(); 
	columnResizing = false;
	couldStartResize = false;
	resizepoint = null; 
}
function getMinLeft(td, table)
{
	var leftmin = 9;
	if(td.getAttribute("type") == "ch")
		leftmin += table.getHeaderWidth(); 
	if(td.getAttribute("pec") != null && td.getAttribute("type") != "ch")
		leftmin += 18; 
	return leftmin; 
}
function getMinimalWidth(td)
{
	var _sort = getSortWidth(td); 
	if(_sort == 0)
		return 8;
	else
		return _sort; 		
}
function fixRightToLeftScroll()
{
	var x = 0; 
	if(document.body.currentStyle.overflowY != "hidden" && document.body.currentStyle.overflowY != "visible")
	{
		if((document.body.currentStyle.overflowY == "auto" && document.body.scrollHeight >= document.body.offsetHeight) || document.body.currentStyle.overflowY != "auto")
			x = 17;
	}
	return x;
}
function showColumnResizeLine()
{	
	if(resizeline != null)
	{		
		var _gridextable = currheader.getGridEXTable(); 
		var _gridex = _gridextable.getGridEX(); 
		var _htmltable = _gridextable.getHtmlItemsTable().parentElement;		
		var divgridex = _gridex.getHtmlGridEX();
		var divtable = _gridextable.getHtmlDiv();
		var htmlheader = currheader.getHtmlHeader();
		var _rtl = divgridex.getAttribute("rtl") == "1"; 
		var leftmin = -1;
		if(_rtl)		
			leftmin = (getPixelLeft(currcolumn) + currcolumn.offsetWidth) - 9; 
		else
			leftmin = (currcolumn.offsetLeft + 9)  + (divtable.offsetLeft + divgridex.offsetLeft + htmlheader.offsetParent.offsetLeft); 
		
		if(!currheader.getIsRoot() && !_rtl)
			leftmin -= _gridextable.getHtmlItemsTable().offsetParent.scrollLeft;
		else if(!currheader.getIsRoot())
		{
			leftmin += (_gridextable.getHtmlItemsTable().offsetParent.scrollWidth - _gridextable.getHtmlItemsTable().offsetParent.offsetWidth) - _gridextable.getHtmlItemsTable().offsetParent.scrollLeft;
			if(_gridex.getRootTable().getHtmlItemsTable().offsetLeft >= 0)
				leftmin += (_gridex.getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - _gridex.getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (_gridex.getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - _gridex.getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
		}			
		if(currcolumn.type != null && currcolumn.type == "ch")
			leftmin += _gridextable.getHeaderWidth();
			
		if(currcolumn.getAttribute("pec") != null && currcolumn.type != "ch")
			leftmin += 18;	
			
		if(_rtl)
		{
			var _l = window.event.clientX;
			_l -= fixRightToLeftScroll();
			_l -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft); 
			if(_l >= leftmin)
				return; 
		}
		else
		{
			if(window.event.clientX + getScrollLeft(_gridex) <= leftmin)
				return;
		}			
		var top = getPixelTop(currcolumn);
		if(!currheader.getIsRoot())
			top -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollTop;
			
		var left = event.clientX - 2;
		left += getScrollLeft(_gridex); 		
		if(_rtl)
		{
			left -= fixRightToLeftScroll();
			left -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft);
		}
		var height = _gridextable.getHtmlItemsTable().offsetParent.offsetHeight; 		
		height -=	(top - getPixelTop(_gridextable.getHtmlItemsTable().offsetParent));
		drawResizeLine(left, top, height); 
	}					
}
function hideColumnResizeLine()
{
	if(resizeline != null)
	{
		resizeline.style.display = "none";
		resizeline.style.visibility = "hidden";
	}
}
function cancelCurrentUIEvents()
{
	if(columnResizing)
		cancelColumnResize(); 
	if(columnDraging)
		cancelColumnDraging(); 
	
	if(currpressedcolumn != null)	
		ShowColumnUnPressed(); 
		
	currpressedcolumn = null;			
	couldStartDrag = false; 
	couldDragColumn = null; 
	couldDragHeader = null;
	dragpoint = null;	
	canceledByUser = true; 
}
function loadAdditionalElements(gridex)
{	
	if(resizeline == null)
	{
		resizeline = document.createElement("DIV");
		resizeline.id = "resizeline";		
		resizeline.style.backgroundColor = "black"; 
		resizeline.style.pixelWidth = 1;
		resizeline.style.position = "absolute";
		resizeline.style.overflow = "hidden"; 
		resizeline.style.display = "none";
		resizeline.style.visibility = "hidden"; 		
		resizeline.style.zIndex = 3001; 
		resizeline.style.cursor = cursorResize;
		resizeline.attachEvent("onmouseup", resizeline_onmouseup); 
		document.body.appendChild(resizeline);		
	}	
	if(dragcolumn == null)
	{			
		dragcolumn = document.createElement("DIV");
		dragcolumn.id = "dragcolumn"; 
		dragcolumn.varX = -1; 
		dragcolumn.style.cursor = "default";	
		dragcolumn.style.position = "absolute";
		dragcolumn.style.pixelHeight = 1; 
		dragcolumn.style.display = "none";
		dragcolumn.style.visibility = "hidden";
		dragcolumn.style.overflow = "hidden";		
		dragcolumn.style.zIndex = 3001;	
		dragcolumn.attachEvent("onselectstart", drag_onselectstart); 
		dragcolumn.attachEvent("onmousemove", drag_onmousemove); 
		dragcolumn.attachEvent("onmouseup", drag_onmouseup); 
		document.body.appendChild(dragcolumn); 					
	}			
	var index = 0;
	while(index < 5)
	{			
		var line = document.getElementById("ua" + (index+1)); 
		if(line == null)
		{
			line = document.createElement("DIV");
			line.id="ua" + (index+1);
			line.style.zIndex = 3000;
			line.style.position = "absolute"; 
			line.style.backgroundColor = "red";
			line.style.fontSize = "1px";
			line.style.pixelWidth = (index + 1) * 2;
			line.style.pixelHeight = 1;
			line.style.padding = "0px 0px"; 
			line.style.overflow = "hidden"; 
			line.style.display = "none"; 
			line.style.visibility  = "hidden";
			document.body.appendChild(line);			
		}				
		line = document.getElementById("da" + (index+1)); 
		if(line == null)
		{
			line = document.createElement("DIV");
			line.id = "da" + (index+1); 
			line.style.zIndex = 3000; 
			line.style.position = "absolute"; 
			line.style.backgroundColor = "red";
			line.style.fontSize = "1px"; 				
			line.style.pixelWidth = (index + 1) * 2;
			line.style.pixelHeight = 1;
			line.style.padding = "0px 0px"; 
			line.style.overflow = "hidden"; 
			line.style.display = "none"; 
			line.style.visibility = "hidden";						
			document.body.appendChild(line); 
		}			
		index++;
	}		
	var arrow = document.getElementById("ua" + (index+1)); 
	if(arrow == null)
	{
		arrow = document.createElement("DIV");
		arrow.id  = "ua" + (index+1);
		arrow.style.zIndex = 3000;
		arrow.style.backgroundColor = "red";
		arrow.style.fontSize = "1px"; 
		arrow.style.padding = "0px 0px";
		arrow.style.overflow = "hidden"; 
		arrow.style.position = "absolute";
		arrow.style.pixelWidth = 4;
		arrow.style.pixelHeight = 4;
		arrow.style.display = "none"; 
		arrow.style.visibility = "hidden"; 
		document.body.appendChild(arrow); 
	}	
	arrow = document.getElementById("da" + (index+1)); 
	if(arrow == null)
	{
		arrow = document.createElement("DIV"); 
		arrow.id = "da" + (index+1);
		arrow.style.zIndex = 3000;
		arrow.style.backgroundColor = "red"; 
		arrow.style.fontSize = "1px"; 
		arrow.style.overflow = "hidden";
		arrow.style.padding = "0px 0px"; 
		arrow.style.position = "absolute";
		arrow.style.pixelHeight = 4;
		arrow.style.pixelWidth = 4;
		arrow.style.display = "none"; 
		arrow.style.visibility = "hidden"; 
		arrow.style.zIndex = 3002; 
		document.body.appendChild(arrow); 
	}			
	if(document.getElementById("colpressed") == null)
	{
		var _tdpressed = document.createElement("DIV");
		_tdpressed.id = "colpressed";
		_tdpressed.style.display = "none"; 
		_tdpressed.style.visibility = "hidden";
		_tdpressed.innerHTML = "<span>&nbsp;</span>"; 
		document.body.appendChild(_tdpressed);
	}	
}
function showResizeLine()
{
	if(columnresizeMode == 1)	
		showColumnResizeLine();
	else if(columnresizeMode == 2)	
		showColumnSetResizeLine(); 
}
function cancelResizeLine()
{
	if(columnresizeMode == 1)
		cancelColumnResize();
	else if(columnresizeMode == 2)
		cancelColumnSetResize(); 
}
function gridex_columnbuttonclick(gridexid, tableid, colindex, serverside)
{		
	window.setTimeout(before_columnbuttonclick(gridexid, tableid, colindex, serverside), 200);
}
function after_columnbuttonclick(gridexid, tableid, colindex, serverside)
{
	var _gridex =getGridEXFromID(gridexid);
	if(_gridex.getGridEXRow() == null)
		return; 
		
	var _table = _gridex.getTables().getTableByID(tableid); 			
	var _column = _table.getColumns().getGridEXColumn(colindex);
	_gridex.FireEvent("ColumnButtonClick", [_column]);
	if(serverside == 1)
	{
		var _row = _gridex.getGridEXRow();
		var arguments = _row.getID() + ":" + colindex;
		_gridex.DoPostBack(null, "ColumnButtonClick:"+arguments); 
	}
}
function before_columnbuttonclick(gridexid, tableid, colindex, serverside)
{
	return "after_columnbuttonclick('" + gridexid + "','" + tableid + "'," + colindex + "," + serverside + ")";
}
function getMinimalColumnSetsWidth(columnset)
{	
	var _columnsets = columnset.getGridEXHeader().getColumnSets(); 
	var _columnsetslength = _columnsets.getCount();
	var _columnset = null; 	
	var _cellslength = -1;
	var _htmlcolumnset = null; 
	var _minimalArray = new Array(); 
	var _minimalcols = null; 
	var _minimalwidth = 0; 
	for(var _index = 0; _index < _columnsetslength; _index++)
	{		
		if(_index != columnset.getIndex())
		{		
			_columnset = _columnsets.getColumnSetInIndex(_index); 
			_htmlcolumnset = _columnset.getHtmlColumnSet(); 			
			_cellslength = _htmlcolumnset.cells.length; 
			_minimalcols = new Array(); 
			for(var _icell = 0; _icell < _cellslength; _icell++)
			{				
				_cell = _htmlcolumnset.cells[_icell]; 
				if(_cell.type != "space")
				{
					if(_cell.type != "header") 
					{
						_minimalsize = 8;						
						if(_cell.type == "ch")
							_minimalsize += _columnset.getGridEXTable().getHeaderWidth();
						_minimalsize += getSortWidth(_cell); 					
						if(_minimalcols[_cell.usecol] != null)
						{
							if(_minimalsize >= _minimalcols[_cell.usecol])
								_minimalcols[_cell.usecol] = _minimalsize; 
						}
						else
							_minimalcols[_cell.usecol] = _minimalsize; 
					}
				}
			}
			_minimalwidth = 0; 
			for(var _icol = 0; _icol < _minimalcols.length; _icol++)
				_minimalwidth += _minimalcols[_icol]; 
			
			_minimalArray[_index] = _minimalwidth; 
		}
		else 
			_minimalArray[_index] = -1; 
	}
	return _minimalArray; 
}
function getDivRoot(htmlDiv, divType)
{
	var _element = null; 
	var _divChildNodesLength = htmlDiv.childNodes.length; 
	for(var _indexDiv = 0; _indexDiv < _divChildNodesLength; _indexDiv++)
	{
		_element = htmlDiv.childNodes[_indexDiv]; 
		if(_element.tagName =="DIV" && _element.type != null && parseInt(_element.type, 10) == divType)
			return _element; 			
	}				
	return null;
}
function getScrollLeft(gridex)
{
	return gridex.getHtmlGridEX().offsetParent.scrollLeft; 	
}
function getRequiredScrollLeft(gridex)
{
	var scrollLeft = 0;
	if(gridex != null)
	{
		if(gridex.getHtmlGridEX().offsetParent != document.body)
		{
			scrollLeft += getScrollLeft(gridex);
			scrollLeft -= document.body.scrollLeft;
		}
		else
			scrollLeft += document.body.scrollLeft;
	}
	else		
		scrollLeft += document.body.scrollLeft;
	return scrollLeft;
}
function getRequiredScrollTop(gridex)
{
	var scrollTop = 0;
	if(gridex != null)
	{
		if(gridex.getHtmlGridEX().offsetParent != document.body)
		{
			scrollTop += gridex.getHtmlGridEX().offsetParent.scrollTop;
			scrollTop -= document.body.scrollTop;
		}
		else
			scrollTop += document.body.scrollTop;
	}
	else	
		scrollTop += document.body.scrollTop;		
	return scrollTop;
}
function resetRootTableScroll(rootTable)
{	
	if(rootTable == null)
		return; 
		
	var _rtl = rootTable.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1";
	var _t = rootTable.getHtmlItemsTable(); 
	var _s = _t.offsetParent.scrollLeft;
	if(_s >= 0)
	{
		var _h = getDivRoot(rootTable.getHtmlDiv(), 1); 
		if(_h != null)
		{	
			if(_rtl)
			{
				_s = (_t.offsetParent.scrollWidth - _t.offsetParent.clientWidth) - _s;
				_s *= -1;
				_h.childNodes[0].style.pixelLeft = (_s * -1); 
			}
			else
				_h.style.pixelLeft = (_s * -1);	
		}
		var _th = getDivRoot(rootTable.getHtmlDiv(), 2);
		if(_th != null)
			_th.style.pixelLeft = (_s * -1); 		
		var _nr = getDivRoot(rootTable.getHtmlDiv(), 7);
		if(_nr != null)			
			_nr.style.pixelLeft = (_s * -1); 		
		var _fr = getDivRoot(rootTable.getHtmlDiv(), 9)
		if(_fr != null)
			_fr.style.pixelLeft = (_s * -1);
	}
}
function fixTableSize(rootTable)
{
	var _table = rootTable.getHtmlItemsTable(); 
	var _headers = getDivRoot(rootTable.getHtmlDiv(), 1); 
	if(_headers != null)		
		_table.style.pixelWidth = _headers.childNodes[0].offsetWidth;	
}
function getMinimalColumnSetsCoreWidth(minimalArray, excludeIndex)
{	
	var _minimalWidth = 0; 
	for(var _index = 0; _index < minimalArray.length; _index++)
	{
		if(_index != excludeIndex)
			_minimalWidth += minimalArray[_index]; 
	}
	return _minimalWidth; 
}
function ShowColumnPressed(column, pressed)	
{	
	var _colpressed = document.getElementById("colpressed");	
	if(column.type != null && column.type == "header") 
	{
		_colpressed.style.borderLeft = column.style.borderLeft;
		_colpressed.style.borderRight = column.style.borderRight;
		_colpressed.style.borderTop = column.style.borderTop; 
		_colpressed.style.borderBottom = column.style.borderBottom; 
	}
	else
	{
		_colpressed.style.borderLeft = column.currentStyle.borderLeftWidth + " " + column.currentStyle.borderLeftStyle + " " + column.currentStyle.borderLeftColor; 
		_colpressed.style.borderRight = column.currentStyle.borderRightWidth + " " + column.currentStyle.borderRightStyle + " " + column.currentStyle.borderRightColor;  
		_colpressed.style.borderTop = column.currentStyle.borderTopWidth + " " + column.currentStyle.borderTopStyle + " " + column.currentStyle.borderTopColor; 
		_colpressed.style.borderBottom = column.currentStyle.borderBottomWidth + " " + column.currentStyle.borderBottomStyle + " " + column.currentStyle.borderBottomColor; 
	}	
	var _colspan = column.childNodes[0];	
	var _colspanpressed = _colpressed.childNodes[0];			
	_colspanpressed.style.borderLeft =  _colspan.style.borderLeft;	
	_colspanpressed.style.borderRight = _colspan.style.borderRight;	
	_colspanpressed.style.borderTop = _colspan.style.borderTop;		
	_colspanpressed.style.borderBottom = _colspan.style.borderBottom;	
	column.style.borderLeftColor = "graytext";
	column.style.borderRightColor = "graytext";
	column.style.borderTopColor = "graytext";
	column.style.borderBottomColor = "graytext";	
	_colspan.style.borderLeftColor = column.offsetParent.currentStyle.backgroundColor;	// column.offsetParent.style.backgroundColor;
	_colspan.style.borderRightColor = column.offsetParent.currentStyle.backgroundColor;
	_colspan.style.borderTopColor = column.offsetParent.currentStyle.backgroundColor;
	_colspan.style.borderBottomColor = column.offsetParent.currentStyle.backgroundColor;		
	currpressedcolumn = column; 
}	
function ShowColumnUnPressed()
{
	if(currpressedcolumn != null)
	{
		var _colpressed = document.all("colpressed"); 
		currpressedcolumn.style.borderLeftColor = _colpressed.style.borderLeftColor;
		currpressedcolumn.style.borderRightColor = _colpressed.style.borderRightColor;
		currpressedcolumn.style.borderTopColor = _colpressed.style.borderTopColor;
		currpressedcolumn.style.borderBottomColor = _colpressed.style.borderBottomColor;		
		var _colspanpressed = _colpressed.children(0); 		
		var _colspan = _colpressed.children(0); 
		_colspan.style.borderLeftColor = _colspanpressed.style.borderLeftColor;
		_colspan.style.borderRightColor = _colspanpressed.style.borderRightColor;
		_colspan.style.borderTopColor = _colspanpressed.style.borderTopColor;
		_colspan.style.borderBottomColor = _colspanpressed.style.borderBottomColor;
	}
}
function getGridEXOffsetParent(gridex)
{
	if(gridex.parentElement.tagName == "DIV")
		return gridex.parentElement;	
	else if(gridex.offsetParent.tagName == "BODY")
		return gridex.offsetParent;
	else if(gridex.parentElement.tagName != "FORM")
		return gridex.parentElement; 
	else 
		return gridex.offsetParent; 
}
function getBorderStyleWidth(border)
{
	var width = 0;			
	if(border != null && border != "")
	{
		var indexOf = border.indexOf("px");
		if(indexOf > 0)
			width += parseInt(border.substr(0, indexOf));		
	}
	return width; 
}
function getRealCellWidth(_spancell, cell, gridexID)
{		    
	_spancell.style.paddingLeft = cell.childNodes[0].currentStyle.paddingLeft; 
	_spancell.style.paddingRight = cell.childNodes[0].currentStyle.paddingRight;
	var _childlength = cell.childNodes.length; 
	var _array = new Array(_childlength); 
	for(var _ichild = 0; _ichild < _childlength; _ichild++)
	{	
		_child = cell.childNodes[_ichild]; 
		_array[_ichild] = _child.innerHTML; 		
	}
	_spancell.innerHTML = _array.join(); 				
	return  _spancell.offsetWidth; 
}
function getColumnFromElement(element)
{
	while(element.tagName != "TD")
		element = element.parentElement;
			
	return element;
}
function getPadding(padding)
{
	var pixelpadding = 0;
	var indexof = padding.indexOf("px");
	if(indexof > 0)
		pixelpadding = parseInt(padding.substr(0, indexof));		
	return pixelpadding;
}
function getPaddingBottom(item)
{
	var _style = null;
	_style = item.style; 
	if(_style.paddingBottom != "")
		return getPadding(_style.paddingBottom);	
		
	_style = item.currentStyle;
	if(_style.paddingBottom != "")
		return getPadding(_style.paddingBottom);
		
	return 0;
}
function getPaddingTop(item)
{
	var _style = null;
	_style = item.style; 
	if(_style.paddingTop != "")
		return getPadding(_style.paddingTop);	
		
	_style = item.currentStyle;
	if(_style.paddingTop != "")
		return getPadding(_style.paddingTop);
		
	return 0;	
}
function getPaddingLeft(item)
{	
	var _style = null;
	_style = item.style; 
	if(_style.paddingLeft != "")
		return getPadding(_style.paddingLeft);	
		
	_style = item.currentStyle;
	if(_style.paddingLeft != "")
		return getPadding(_style.paddingLeft);
		
	return 0;
}
function getStylePaddingLeft(style)
{
	if(style.paddingLeft != "")
		return getPadding(style.paddingLeft); 
		
	return 0; 
}
function getPaddingRight(item)
{
	var _style = null;
	_style = item.style;
	if(_style.paddingRight != "")
		return  getPadding(_style.paddingRight);	
		
	_style = item.currentStyle; 
	if(_style.paddingRight != "")
		return getPadding(_style.paddingRight); 
	
	return 0;
}
function getStylePaddingRight(style)
{
	if(style.paddingRight != "")
		return getPadding(style.paddingRight); 
		
	return 0; 
}
function getBorderTopWidth(td)
{
	var width = 0;
	if(td.style.borderTopWidth != "")
		width = getBorderStyleWidth(td.style.borderTopWidth); 
	else if(td.currentStyle.borderTopWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderTopWidth); 
	return width; 	
}
function getBorderBottomWidth(td)
{
	var width = 0; 
	if(td.style.borderBottomWidth != "")
		width = getBorderStyleWidth(td.style.borderBottomWidth);
	else if(td.currentStyle.borderBottomWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderBottomWidth); 
	return width; 
}
function getBorderLeftWidth(td)
{
	var width = 0;
	if(td.style.borderLeftWidth  != "")
		width = getBorderStyleWidth(td.style.borderLeftWidth); 
	else if(td.currentStyle.borderLeftWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderLeftWidth);
	return width;
}
function getBorderRightWidth(td)
{
	var width = 0; 
	if(td.style.borderRightWidth != "")
		width = getBorderStyleWidth(td.style.borderRightWidth);
	else if(td.currentStyle.borderRightWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderRightWidth); 
	return width; 
}
function getElementWidth(element)
{
	var width = 0; 
	width = element.style.pixelWidth; 
	if(width == 0)		
	{
		if(element.currentStyle.width != "")
			width = getPixelWidth(element.currentStyle.width); 
	}
	return width; 
}
function getStyleBorderWidth(style)
{	
	var width = 0; 
	if(style.borderLeftWidth != "")
	{
		var indexOf = style.borderLeftWidth.indexOf("px"); 
		if(indexOf > 0)
			width += parseInt(style.borderLeftWidth.substr(0, indexOf)); 
	}	
	if(style.borderRightWidth != "")
	{
		var indexOf = style.borderRightWidth.indexOf("px");
		if(indexOf > 0)
			width += parseInt(style.borderRightWidth.substr(0, indexOf)); 		
	}
	return width; 
}
function getBorderWidth(td)
{
	var width = 0;	
	if(td.style.borderLeftWidth != "")
	{
		var indexOf = td.style.borderLeftWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.style.borderLeftWidth.substr(0, indexOf)); 
	}
	else if(td.currentStyle.borderLeftWidth != "")
	{
		var indexOf = td.currentStyle.borderLeftWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.currentStyle.borderLeftWidth.substr(0, indexOf));
	}	
	if(td.style.borderRightWidth != "")
	{
		var indexOf = td.style.borderRightWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.style.borderRightWidth.substr(0, indexOf)); 
	}
	else if(td.currentStyle.borderRightWidth != "")
	{
		var indexOf = td.currentStyle.borderRightWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.currentStyle.borderRightWidth.substr(0, indexOf)); 
	}
	return width; 
}
function isInResizeArea(td, htmlTable, table)
{
	var x = window.event.offsetX;
	var y = window.event.offsetY;		
	var ylow = 0;
	var yhigh = ylow + td.offsetHeight; 		
	var xlow;
	var xhigh; 				
	var tdleft = td.offsetLeft; 				
	if(tdleft + td.offsetWidth + 2 <= htmlTable.offsetWidth)
	{
		xlow = td.offsetWidth - 3;	
		xhigh = td.offsetWidth + 3; 		
	}
	else
	{
		xlow = td.offsetWidth - 4;
		xhigh = td.offsetWidth; 
	}			
	if((x >= xlow && x <= xhigh) && (y >= ylow && y <= yhigh))
		return true;
	else
		return false;
}
function getTypeOfTD(element)
{
	if(element == null)	
		return ""; 
		
	while(element != null && element.tagName != "TD")
		element = element.parentElement; 
		
	if(element != null && element.tagName == "TD")
		return element.getAttribute("type"); 
	else
		return ""; 
}
function getPreviewClassName(row)
{
	if(row.getIsAlternating())
		return row.getTable().getRowCss(4); 
	else
		return row.getTable().getRowCss(5); 
}	
function getPreviewSelectedClassName(row)
{
	return row.getTable().getRowCss(6); 
}	
function getClassName(row)
{
	var className = "";
	className = row.getGridEX().getClassName(row.getID()); 
	if(className != null)
		return className; 
		
	var rowType = row.getType(); 
	switch(rowType)
	{
		case 3:
		{
			if(!row.getIsAlternating())
				className = row.getTable().getRowCss(0); 
			else
				className = row.getTable().getRowCss(2); 
		} break;			
		case 5:
			className = row.getTable().getRowCss(7); 
		break; 			
		case 8:			
			className = row.getTable().getRowCss(14); 
			break; 			 
		case 9:
			className = row.getTable().getRowCss(20); 
		break; 			
		case 11:
			className = row.getTable().getRowCss(26); 
		break;
		case 12:
			className = row.getTable().getRowCss(28); 
		break; 			
		default:
			className = row.getTable().getRowCss(0); 
		break; 
	}		
	return className; 
}			
function getSelectedClassName(row)
{
	var className = ""; 
	className = row.getGridEX().getSelectedClassName(row.getID());
	if(className != null)
		return className; 
		
	var rowType = row.getType(); 
	switch(rowType)
	{
		case 3:
		{
			if(!row.getIsAlternating())
				className = row.getTable().getRowCss(1); 
			else
				className = row.getTable().getRowCss(3); 
		} break; 			
		case 8:			
			className = row.getTable().getRowCss(30); 
		break; 									
		default:
			className = row.getTable().getRowCss(1); 
		break; 
	}		
	return className; 
}
function getColumnIDFromCellID(cellID)
{
	var _indexof = cellID.indexOf("_L"); 
	if(_indexof > 0)
		return cellID.substring(0, _indexof); 

	return ""; 
}
function getGridEXFromID(id)
{
	var _gridex = null;
	eval("_gridex = " + id + ";"); 
	if(_gridex == null) 
		throw Error("GridEX '" + id + "' object is null or invalid"); 
		
	return _gridex; 
}
function getGridEXCalendarFromID(id)
{
	var _calendar = null; 
	eval("_calendar = " + id + ";"); 
	if(_calendar == null)
		throw Error("GridEXCalendar '" + id + "' object is null or invalid"); 
		
	return _calendar; 
}
function getHorizontalScrollOffset(gridex)
{
	var _offset = 0;
	var _table = gridex.getRootTable();
	_offset = _table.getHtmlItemsTable().offsetParent.scrollLeft;
	return _offset; 
}
function getVerticalScrollOffset(gridex)
{	
	var _offset = 0; 
	var _table = gridex.getRootTable(); 	
	_offset = _table.getHtmlItemsTable().offsetParent.scrollTop; 
	return _offset; 
}
function getPixelTop(element)
{
	var top = 0;
	while(element != null)
	{					
		top += element.offsetTop;
		element = element.offsetParent; 				
	}
	return top; 
}
function getAdjustPixelTop(gridex, excludeBody, excludeTable)
{
	var adjust = 0;
	if(excludeBody == null || !excludeBody)
		adjust += document.body.scrollTop;
		
	if(excludeTable == null || !excludeTable)
		adjust += gridex.getRootTable().getHtmlItemsTable().offsetParent.scrollTop;	
		
	return adjust;
}
function getAvailableClientHeight(element)
{
	var height = document.body.clientHeight;	
	return height; 
}
function getBottomOffset(element)
{
	var offset = 0; 
	offset += getPixelWidth(element.currentStyle.marginBottom); 
	return offset; 
}
function getTopOffset(element)
{
	var offset = 0; 
	offset += getPixelWidth(element.currentStyle.marginTop);
	return offset;
}
function getOffsetTopForEdit(gridEXHtml)
{
	var offset = 0; 
	if(gridEXHtml.currentStyle.position == "relative")
	{
		var _offsetParent = getGridEXOffsetParent(gridEXHtml); 
		if(_offsetParent.tagName != "BODY")					
			offset += getPixelTop(_offsetParent); 
	}
	return offset; 
}
function getOffsetLeftForEdit(gridEXHtml)
{
	var offset = 0; 
	if(gridEXHtml.currentStyle.position == "relative")
	{
		var _offsetParent = getGridEXOffsetParent(gridEXHtml); 
		if(_offsetParent.tagName != "BODY")
			offset += getPixelLeft(_offsetParent); 
	}
	return offset; 
}
function getRTLScrollWidth(element)
{
	if(element.offsetParent.scrollHeight >= element.offsetParent.clientHeight)
	{
		if(element.offsetLeft <= 0)
			return 17;
	}
	return 0; 
}
function getPixelLeft(element, rtl)
{
	var left = 0; 
	while(element != null)
	{		
		if(rtl == null || !rtl)
			left += element.offsetLeft;		
		else if(rtl != null && rtl)
		{
			if(element.offsetLeft > 0)
				left += element.offsetLeft; 
		}
		element = element.offsetParent; 
	}
	return left; 
}
function getPercentWidth(width)
{
	var percentage = 0;
	if(width.indexOf("%") > 0)
		return parseInt(width.substring(0, width.indexOf("%")), 10);
		
	return 0; 
}
function getPixelWidth(width)
{
	var pixel = 0; 
	if(width.indexOf("px") > 0)
		return parseInt(width.substring(0, width.indexOf("px")), 10); 

	return 0; 
}
function getPixelColWidth(width)
{
	if(width == null || width.length == 0)
		return 0; 
		
	if(width.indexOf("px") > 0)
		return getPixelWidth(width); 
		
	return parseInt(width, 10); 
}
function getRootRowFromInner(element)
{
	while(element != null)
	{
		if(element.nodeType == 1 && element.tagName == "TR" && element.getAttribute("id") != null && element.getAttribute("t") != null)
			return element; 
		
		element = element.parentElement; 
	}
	if(element == null)
		throw Error("unable to find root row"); 
}
function getHierarchicalRow(element)
{	
	var isrow = false; 
	do
	{	
		if(element == null)
			throw Error("hierarchical or grouped row is not found"); 
	
		if(element.tagName == "TR" && element.id != null && element.t != null) 
			isrow = true;
		else
			element = element.parentElement;
		
	} while(!isrow);
	return element; 	
}
function getHierarchicalRowTop(element)
{	
	element = getHierarchicalRow(element); 		
	return element.offsetTop;	
}
function getPositionOfCell(cellindex, cells)
{
	for(var index = 0; index < cells.length; index = index + 2)
	{
		if(cells[index] == cellindex) 
			return index;
	}		
	return -1; 
}
function getTextNode(node)
{	
	if(node.nodeType == 3)
		return node;
	else
	{
		for(var i=0;i<node.childNodes.length;i++)
		{
			var anode = getTextNode(node.childNodes[i]);
			if(anode != null)
				return anode;				
		}
		return null;
	}
}
function getSortWidth(column)
{		
	if(column.childNodes.length == 2)
		return column.childNodes[1].offsetWidth; 
	else
		return 0; 
}
function replaceInstances(value, find, replace)
{
	while(value.indexOf(find) >= 0)
	{
		value = value.replace(find, replace);
	}
	return value; 
}
function normalizeValue(value)
{
	if(value != null && typeof(value) == "string")
	{
		value = replaceInstances(value, "&ent;", "\r\n");
		value = replaceInstances(value, "&quot;", "\"");		
		value = replaceInstances(value, "&apos;", "'"); 
	}
	return value;	
}
function updateColumnDefinitionInField(field, cellID, cellPos, cellWidth, innerWidth)
{		
	var _exists = false; 	
	var _colsdef = null; 
	if(field.value == null || field.value.length == 0)
		_colsdef = new Array(); 
	else
		_colsdef = field.value.split(","); 
	
	var _newpos = null;
	var _newwidth = null; 
	var l = _colsdef.length; 	
	if(l > 0)
	{		
		var _coldef = null; 	
		var _change = false; 
		for(var i = 0; i < l; i++)
		{
			_coldef = _colsdef[i].split(":"); 				
			if(_coldef[0] == cellID)
			{				
				_exists = true; 
				_change = false;
				if(parseInt(_coldef[1],10) != cellPos)
					_change = true; 
					
				if(parseInt(_coldef[2], 10) != cellWidth)
					_change = true; 
					
				if(innerWidth != null && parseInt(_coldef[3], 10) != innerWidth)
					_change = true; 
					
				if(_change)
				{
					_newpos = (parseInt(_coldef[1],10) != cellPos) ? cellPos : _coldef[1]; 					
					if(cellWidth != -1 && cellWidth != parseInt(_coldef[2], 10))
						_newwidth = cellWidth;
					else
						_newwidth = parseInt(_coldef[2], 10);
					var _item = 
					_colsdef[i] = cellID + ":" + _newpos + ":" + _newwidth; 
					if(innerWidth != null)
						_colsdef[i] += ":" + innerWidth; 
				}
			}
		}		
		if(!_exists)
		{
			var _item = cellID + ":" + cellPos + ":" + cellWidth;
			if(innerWidth != null)
				_item += ":" + innerWidth; 
			_colsdef[_colsdef.length] = _item; 			
		}
	}
	else
	{	
		var _item = cellID + ":" + cellPos + ":" + cellWidth;
		if(innerWidth != null)
			_item += ":" + innerWidth; 
		_colsdef[_colsdef.length] = _item; 
	}	
	field.value = _colsdef.join(","); 
}
function setGroupEventData(gridexID, gaction, gtable, gpos, gnewpos, gcolumn, hcolumnpos)
{		
	var input = document.getElementById(gridexID + "_eventdata"); 
	if(input == null)
		throw Error("event data field is null"); 
			
	input.value = gaction + ":" + gtable + ":" + gpos + ":" + gnewpos + ":" + gcolumn + ":" + hcolumnpos; 
}
function rebuildArray(array)
{
	var j = 0;
	for(var i=0;i<array.length;i++)
	{
		if(array[i] != null)
		{
			array[j] = array[i];
			j++;
		}
	}
	array.length = j;
}
function unloadArray(array)
{
	if(array != null)
	{
		for(var i = 0; i < array.length; i++)
		{
			var o = array[i];			
			delete o; 
			o = null;
		}
	}	
}
function unloadObjectsArray(array)
{
	if(array != null)
	{
		for(var i=0;i<array.length;i++)
		{
			var object = array[i];
			if(object.Unload != null)
				object.Unload();
			delete object; 
			object = null;			
		}		
	}
}
function unloadRows(rows)
{
	if(rows != null)
	{
		for(var i=0;i<rows.length;i++)
		{
			var r=rows[i];
			r.Unload();
			delete r; 
			r = null;
		}		
	}	
}
function cancelEvent()
{
	window.event.returnValue = false;
	window.event.cancelBubble = true;
	return false;
}