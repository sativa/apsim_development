//////////////////////////////////////////////////////////////////
// GridEX JavaScript MZ API 1.1.1009
// Copyright by Janus Systems S.A.
// 2002 - 2004
//////////////////////////////////////////////////////////////////
var rv = null; 
var dragcolumn = null; 
var resizeline = null;  
var canceledByUser = false; 
var columnResizing = false; 
var columnDraging = false; 
var couldDragColumn = null;
var couldDragHeader = null; 
var couldResizeHeader = null; 
var couldResizeColumnSet = null; 
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
var lastselKey = -1; 
var leftover = null;
var rightover = null; 
var bottomover = null;
var topover = null; 
var divtt = null;
var isDefaultView = (document.defaultView != null && document.defaultView.getComputedStyle != null); 
function Point(x, y)
{
	var x = x;
	var y = y; 	
	this.X = X;
	this.Y = Y;	
	function X()
	{
		return x; 
	}	
	function Y()
	{
		return y; 
	}
}
function PasswordText(text, passwordChar)
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
function showthemed_columnover(postop, posleft, posright, posbottom, width, height, showtop, showbottom)
{		
	if(leftover != null)
	{
		leftover.style.pixelTop = postop;
		leftover.style.pixelLeft = posleft;
		leftover.style.pixelHeight = height;
		leftover.style.visibility = "visible";
	}	
	if(showtop == null || showtop)
	{
		if(topover != null)
		{
			topover.style.pixelTop = postop; 
			topover.style.pixelLeft = posleft;
			topover.style.pixelWidth =  width;
			topover.style.visibility = "visible";
		}
	}	
	if(rightover != null) 
	{
		rightover.style.pixelTop = postop;
		rightover.style.pixelLeft = posright;
		rightover.style.pixelHeight = height; 
		rightover.style.visibility = "visible";
	}	
	if(showbottom == null || showbottom)
	{	
		if(bottomover != null)
		{	
			bottomover.style.pixelTop = posbottom; 
			bottomover.style.pixelLeft = posleft;
			bottomover.style.pixelWidth = width; 		
			bottomover.style.visibility = "visible";			
		}
	}
}
function previewrow_oncollapse(cell, gridexID, tableID)
{
	if(cell.nodeType == 1 && cell.tagName == "TD")
	{
		var _gridex = getGridEXFromID(gridexID); 
		if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
		{
			var _gridexTable = _gridex.getTables().getTableByID(tableID); 			
			var _row = null;
			
			if(_gridexTable.getUseColumnSets())
				_row = cell.offsetParent.offsetParent.parentElement;
			else
				_row = cell.parentElement; 
				
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
function onSelectRow(innerRow, gridex, gridexTable)
{	
	var row  = gridex.RetrieveRow(getRootRowFromInner(innerRow), innerRow, gridexTable);		 
	gridex.setCurrentRow(row); 
	gridex.getSelectedItems().SelectRow(row);
	setRowForEditOrFilter(row)
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
			if(_gridex.getSelectedItems().getSelectedItemInIndex(0).getRow().getID() == _row.getID())
			{		
				setRowForEditOrFilter(_row);
				return false; 
			}
		}				
	}
	if(_row != null)
	{
		_gridex.setCurrentRow(_row, true); 
		_gridex.setHitTestArea(0); 
		_gridex.FireEvent("Click", [_gridex, (window.event.type != null && window.event.type == "contextmenu") ? 2 : 1,window.event.clientX, window.event.clientY]);
		_gridex.FireEvent("SelectionChanged", [_row]); 
		_gridex.DoPostBack(null, "SelectionChanged:"+_row.getID());
	}
}
function row_onclick(row, tableID, gridexID)
{		
	var _gridex = getGridEXFromID(gridexID);
	if(_gridex.getIsInitialized != null && _gridex.getIsInitialized())
	{
		try
		{
			var _gridextable = _gridex.getTables().getTableByID(tableID);			
			onSelectRow(row, _gridex, _gridextable); 
			_gridex.setHitTestArea(0); 
			_gridex.FireEvent("Click", [_gridex, (window.event.type != null && window.event.type == "contextmenu") ? 2 : 1,window.event.clientX, window.event.clientY]);
			if(window.event.type == "contextmenu")
			{
				window.event.cancelBubble = true; 
				window.event.returnValue = false; 
			}
		}
		catch(err) {  }
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
		if(_gridex.dcpb)
			_gridex.DoPostBack(null, "DoubleClick"); 
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
		_gridex.FireEvent("Click", [_gridex,(window.event.type == "contextmenu") ? 2 : 1, window.event.clientX, window.event.clientY]); 		
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
		if(_headers.length > 0)
		{
			for(var iheader = 0; iheader < _headers.length; iheader++)
				_headers[iheader].CheckSelectors(columnID, checkbox.checked, true);
		}
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
function hidethemed_columnover()
{	
	if(leftover != null)
		leftover.style.visibility = "hidden";
		
	if(topover != null)
		topover.style.visibility = "hidden";
		
	if(rightover != null)
		rightover.style.visibility = "hidden";
	
	if(bottomover != null)
		bottomover.style.visibility = "hidden";
}
function drawDownArrow(x, y)
{	
	var index = 0;
	while(index < 5)
	{
		var line = document.getElementById("da" + (index+1));
		if(line !=null)
		{
			line.style.left = (x -  index) + "px";
			line.style.top = (y + index) + "px"; 
			line.style.display = "";
			line.style.visibility = "visible"; 			
			index++;
		}
	}			
	var arrow = document.getElementById("da" + (index+1));
	if(arrow != null)
	{	
		arrow.style.left = (x - 1) + "px";
		arrow.style.top = (y + index) + "px"; 
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
			_style.left = (x - index) + "px";
			_style.top = (y - index) + "px";
			_style.display = ""; 
			_style.visibility = "visible";
			index++; 			
		}
	}	
	var arrow = document.getElementById("ua" + (index+1));
	if(arrow != null)
	{
		_style = arrow.style;
		_style.left = (x - 1) + "px";
		_style.top = (y - 4 - getPixelValue(_style.height)) + "px";
		_style.display = ""; 
		_style.visibility = "visible"; 
	}
}
function hideColumnForDrop()
{	
	for(var index = 1; index <= 6; index++)
	{
		document.getElementById("da" + index).style.display = "none";
		document.getElementById("da" + index).style.visibility = "hidden";
		document.getElementById("ua" + index).style.display = "none";
		document.getElementById("ua" + index).style.visibility = "hidden"; 
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
				if(currheader.getGridEXTable().getColumns().getGridEXColumnByClientID(currcolumn.getAttribute("id")).getAllowGroup())
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
			
		var currstyle = null;
		if(isDefaultView)
			currstyle = document.defaultView.getComputedStyle(currcolumn, '');
		
		if(currstyle != null)
		{
			dragcolumn.style.backgroundColor = currstyle.getPropertyValue("background-color"); 
			dragcolumn.style.backgroundImage = currstyle.getPropertyValue("background-image"); 
			dragcolumn.style.color = currstyle.getPropertyValue("color"); 
		}
		else
		{			
			dragcolumn.style.backgroundColor = currcolumn.style.backgroundColor;
			dragcolumn.style.backgroundImage = currcolumn.style.backgroundImage; 
			dragcolumn.style.color = currcolumn.style.color;
		}
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
			if(currstyle != null)
			{
				dragcolumn.style.borderBottom = currstyle.getPropertyValue("border-bottom"); 
				dragcolumn.style.borderRight = currstyle.getPropertyValue("border-right"); 
				dragcolumn.style.borderTop = currstyle.getPropertyValue("border-top"); 
				dragcolumn.style.borderLeft = currstyle.getPropertyValue("border-left"); 
			}
			else
			{
				dragcolumn.style.borderBottom = currcolumn.style.borderBottom;
				dragcolumn.style.borderRight = currstyle.style.borderRight;
				dragcolumn.style.borderTop = currstyle.style.borderTop;
				dragcolumn.style.borderLeft = currstyle.style.borderLeft;
			}
		}		
		if(currstyle != null)
			dragcolumn.style.padding = currstyle.getPropertyValue("padding");		
		else
			dragcolumn.style.padding = currcolumn.style.padding; 
		if(currcolumn.font != null && currcolumn.font != "") 
			dragcolumn.style.font = currcolumn.font;

		if(currstyle != null)
		{
			dragcolumn.style.fontFamily = currstyle.getPropertyValue("font-family");
			dragcolumn.style.fontSize = currstyle.getPropertyValue("font-size");
			dragcolumn.style.fontStyle = currstyle.getPropertyValue("font-style");
			dragcolumn.style.fontVariant = currstyle.getPropertyValue("font-variant");
			dragcolumn.style.fontWeight = currstyle.getPropertyValue("font-weight");
			dragcolumn.style.textAlign = currstyle.getPropertyValue("text-align"); 
		}
		else
		{
			dragcolumn.style.fontFamily = currcolumn.style.fontFamily;
			dragcolumn.style.fontSize = currcolumn.style.fontSize;
			dragcolumn.style.fontStyle = currcolumn.style.fontStyle;
			dragcolumn.style.fontVariant = currcolumn.style.fontVariant;
			dragcolumn.style.fontWeight = currcolumn.style.fontWeight;
			dragcolumn.style.textAlign = currcolumn.style.textAlign;
		}
	}	
	if(dragcolumn.style.visibility == "hidden")
	{				
		dragcolumn.style.width =  (currcolumn.offsetWidth) + "px"; 
		dragcolumn.style.height = (currcolumn.offsetHeight) + "px"; 
		dragcolumn.innerHTML =  currcolumn.childNodes[0].innerHTML; 
		if(isDefaultView)
			dragcolumn.childNodes[1].style.padding = document.defaultView.getComputedStyle(currcolumn, '').getPropertyValue("padding"); 
		else
			dragcolumn.childNodes[1].style.padding 
		var _spanpressed = _colpressed.childNodes[0];		
		var _child = dragcolumn.childNodes[1];		
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
			var _left = getPixelLeft(currcolumn);	
			if(!currheader.getIsRoot())
				_left -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
			dragcolumn.style.left = (_left) + "px";			
			var _top = -1;
			if(currheader.getIsRoot())
				_top = getPixelTop(currcolumn); 
			else
			{	
				var _htmltable = currheader.getGridEXTable().getHtmlItemsTable().parentElement; 				
				_top = currcolumn.offsetTop + _htmltable.offsetTop + _htmltable.offsetParent.offsetTop +  getHierarchicalRowTop(_htmlheader) + _gridex.getPixelTop();
				_top -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollTop; 
			}			
			var _offsety = (_top + currcolumn.offsetHeight) - dragpoint.Y();
			dragcolumn.style.top = (window.event.clientY + _offsety - currcolumn.offsetHeight) + "px";  
			dragcolumn.setAttribute("varX", window.event.clientX - getPixelValue(dragcolumn.style.left)); 
			dragcolumn.setAttribute("varY", window.event.clientY - getPixelValue(dragcolumn.style.top));
		}
		else if(columndragMode == 2)	
		{
			var htmlTable = currcolumnset.getHtmlColumnSet(); 
			var divtable = currcolumnset.getGridEXTable().getHtmlDiv(); 
			var divgridex = currcolumnset.getGridEXTable().getGridEX().getHtmlGridEX();			
			var _left = getPixelLeft(currcolumn);				
			dragcolumn.style.left = (_left) + "px"; 			
			var _top = -1; 
			_top = getPixelTop(currcolumn); 			
			var _offsety = (_top + currcolumn.offsetHeight) - dragpoint.Y(); 			
			dragcolumn.style.top = (window.event.clientY + _offsety - currcolumn.offsetHeight) + "px"; 															
			dragcolumn.setAttribute("varX",  window.event.clientX - getPixelValue(dragcolumn.style.left));
			dragcolumn.setAttribute("varY",  window.event.clientY - getPixelValue(dragcolumn.style.top)); 
		}		
	}
	else
	{
		dragcolumn.style.left = (window.event.clientX - dragcolumn.getAttribute("varX")) + "px";		
		dragcolumn.style.top = (window.event.clientY - dragcolumn.getAttribute("varY")) + "px"; 
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
			
		if(isDefaultView)
		{
			var _computedstyle = document.defaultView.getComputedStyle(currcolumn, null); 			
			dragcolumn.style.fontFamily = _computedstyle.getPropertyValue("font-family"); 
			dragcolumn.style.fontSize = _computedstyle.getPropertyValue("font-size"); 
			dragcolumn.style.fontStyle = _computedstyle.getPropertyValue("font-style"); 
			dragcolumn.style.fontVariant = _computedstyle.getPropertyValue("font-variant"); 
			dragcolumn.style.fontWeight = _computedstyle.getPropertyValue("font-weight"); 			
		}
		else
		{			
			dragcolumn.style.fontFamily = _colstyle.fontFamily; 
			dragcolumn.style.fontSize = _colstyle.fontSize; 
			dragcolumn.style.fontStyle = _colstyle.fontStyle; 
			dragcolumn.style.fontVariant = _colstyle.fontVariant; 
			dragcolumn.style.fontWeight = _colstyle.fontWeight; 			
		}
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
		dragcolumn.style.left = getPixelLeft(currcolumn) + "px"; 
		dragcolumn.style.top = getPixelTop(currcolumn) + "px";
		dragcolumn.setAttribute("varX", window.event.clientX - getPixelValue(dragcolumn.style.left));
		dragcolumn.setAttribute("varY", window.event.clientY - getPixelValue(dragcolumn.style.top)); 
	}
	else
	{
		dragcolumn.style.left = (window.event.clientX - dragcolumn.getAttribute("varX")) + "px";		
		dragcolumn.style.top = (window.event.clientY - dragcolumn.getAttribute("varY")) + "px"; 
	}	
	dragcolumn.style.display = "";
	dragcolumn.style.visibility = "visible";
}
function showColumnDrag(event)
{	
	if(columndragMode == 3)
		showColumnGroupDrag(event); 
	else
		showColumnHeaderDrag(event); 	
}
function cancelEvent()
{
	window.event.returnValue = false;
	window.event.cancelBubble = true; 			
	return false; 
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
function getTableOffsetParent(element)
{
	var _t = element.parentElement;
	var d = false;
	while(!d)
	{
		if(_t == null)
			d = true;
		else
		{
			if(_t.tagName == "TABLE" && _t.getAttribute("id") != null)
				d = true;
			else
				_t = _t.parentElement;
		}
	}
	return _t; 
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
function getRealCellWidth(_spancell, cell, gridexID)
{		    
	var _innerspan = getInnerSpan(cell.childNodes[0]);
	if(isDefaultView)
	{
		_spancell.style.paddingLeft =  document.defaultView.getComputedStyle(_innerspan, null).getPropertyValue("padding-left");   
		_spancell.style.paddingRight = document.defaultView.getComputedStyle(_innerspan, null).getPropertyValue("padding-right");  
	}
	else
	{
		_spancell.style.paddingLeft = _innerspan.style.paddingLeft;
		_spancell.style.paddingRight = _innerspan.style.paddingRight; 
	}
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
function getMaximumColumnSize(gridEXColumn)
{
	var _cell = null; 
	var _cells = document.getChildsById(gridEXColumn.getClientID() + "_L"); 
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
	for(var _icell = 0; _icell < _cellsLength && _cellsScanned < _cellsToCount; _icell++)
	{		
		_cell = _cells[_icell]; 
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
	resizeline.style.display = ""; 
	resizeline.style.visibility = "visible";
	resizeline.style.left = left + "px";
	resizeline.style.top = top + "px"; 	
	resizeline.style.height =  height + "px"; 
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
function updateTableSize(_table, _cols)
{
	if(_cols == null)
		_cols = _table.getElementsByTagName("COL"); 
			
	var _sumwidth = 0; 
	for(var _icol = 0; _icol < _cols.length; _icol++)
		_sumwidth += getPixelColWidth(_cols[_icol].width); 
	_table.style.width = _sumwidth + "px"; 
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
				if(_cell.getAttribute("type") != "space")
				{
					if(_cell.getAttribute("type") != "header") 
					{
						_minimalsize = 8;						
						if(_cell.getAttribute("type") == "ch")
							_minimalsize += _columnset.getGridEXTable().getHeaderWidth();
						_minimalsize += getSortWidth(_cell); 					
						if(_minimalcols[_cell.getAttribute("usecol")] != null)
						{
							if(_minimalsize >= _minimalcols[_cell.getAttribute("usecol")])
								_minimalcols[_cell.getAttribute("usecol")] = _minimalsize; 
						}
						else
							_minimalcols[_cell.getAttribute("usecol")] = _minimalsize; 
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
		if(_element.tagName =="DIV" && _element.getAttribute("type") != null && parseInt(_element.getAttribute("type"), 10) == divType)
			return _element; 			
	}				
	return null;
}
function getScrollLeft(gridex)
{
	return gridex.getHtmlGridEX().offsetParent.scrollLeft; 	
}
function resetRootTableScroll(rootTable)
{
	var _table = rootTable.getHtmlItemsTable(); 
	var _s = _table.offsetParent.scrollLeft;
	if(_s >= 0)
	{
		var _h = getDivRoot(rootTable.getHtmlDiv(), 1); 
		if(_h != null)
			_h.style.left = (_s * -1) + "px";
		var _th = getDivRoot(rootTable.getHtmlDiv(), 2);
		if(_th != null)
			_th.style.left = (_s * -1) + "px"; 		
		var _nr = getDivRoot(rootTable.getHtmlDiv(), 7);
		if(_nr != null)			
			_nr.style.left = (_s * -1) + "px"; 		
		var _fr = getDivRoot(rootTable.getHtmlDiv(), 9)
		if(_fr != null)
			_fr.style.left = (_s * -1) + "px";
	}
}
function fixTableSize(rootTable)
{
	var _table = rootTable.getHtmlItemsTable(); 
	var _headers = getDivRoot(rootTable.getHtmlDiv(), 1); 
	if(_headers != null)		
		_table.style.pixelWidth = _headers.childNodes[0].offsetWidth;	
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
		var leftmin = getPixelLeft(currcolumn) - getHorizontalScrollOffset(_gridex) + 9; 
		if(currcolumn.getAttribute("type") != null && currcolumn.getAttribute("type") == "ch")
			leftmin += _gridexTable.getHeaderWidth(); 
		if(currcolumn.getAttribute("pec") != null && currcolumn.getAttribute("type") != "ch")
			leftmin += 18; 
		if(window.event.clientX + getScrollLeft(_gridex)  <= leftmin)
			return;
			
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
	hidethemed_columnover();
	if(currcolumn != null && currheader != null)
	{			
		currheader.ResizeColumnWidth(currcolumn, window.event.clientX);
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
function showColumnResizeLine()
{	
	if(resizeline != null)
	{		
		var _gridextable = currheader.getGridEXTable(); 
		var _gridex = _gridextable.getGridEX(); 
		var _htmltable = _gridextable.getHtmlItemsTable().parentNode;		
		var divgridex = _gridex.getHtmlGridEX();
		var divtable = _gridextable.getHtmlDiv();
		var htmlheader = currheader.getHtmlHeader();		
		var leftmin = (currcolumn.offsetLeft + 9) + (htmlheader.offsetParent.offsetLeft + divtable.offsetLeft + divgridex.offsetLeft);
		if(!currheader.getIsRoot())
			leftmin -= _gridextable.getHtmlItemsTable().offsetParent.scrollLeft;
			
		if(currcolumn.getAttribute("type") != null && currcolumn.getAttribute("type") == "ch")
			leftmin += _gridextable.getHeaderWidth();
			
		if(currcolumn.getAttribute("pec") != null && currcolumn.getAttribute("type") != "ch")
			leftmin += 18;
			
		if(window.event.clientX + getScrollLeft(_gridex) <= leftmin)
			return;
			
		var top = getPixelTop(currcolumn);	
		if(!currheader.getIsRoot())
			top -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollTop;

		var left = window.event.clientX - 2;
		left += getScrollLeft(_gridex);
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
		resizeline.style.width = "1px";
		resizeline.style.display = "none"; 
		resizeline.style.position = "absolute";
		resizeline.style.visibility = "hidden"; 
		resizeline.style.zIndex = 3001; 
		resizeline.addEventListener("mouseup", resizeline_onmouseup, false); 
		document.body.appendChild(resizeline);
	}	
	if(dragcolumn == null)
	{			
		dragcolumn = document.createElement("DIV");
		dragcolumn.id = "dragcolumn"; 
		dragcolumn.setAttribute("varX", -1);; 
		dragcolumn.style.cursor = "default";	
		dragcolumn.style.position = "absolute";
		dragcolumn.style.display = "none"; 
		dragcolumn.style.visibility = "hidden";
		dragcolumn.style.zIndex = 3001;
		dragcolumn.addEventListener("selectstart", drag_onselectstart, false); 
		dragcolumn.addEventListener("mouseup", drag_onmouseup, false); 
		document.body.appendChild(dragcolumn); 				
	}	
	if(leftover == null)
	{
		leftover = document.createElement("DIV"); 
		leftover.id = "leftover";
		leftover.style.backgroundColor = "#fb8844"; 
		leftover.style.width ="2px";
		leftover.style.fontSize = "1px"; 
		leftover.style.filter = "progid:DXImageTransform.Microsoft.Alpha(Opacity=25, FinishOpacity=100, Style=1, StartY=0, FinishY=100, StartX=0, FinishX=0)"; 		
		leftover.style.zIndex = 3001; 
		leftover.style.display = "none"; 
		leftover.style.position = "absolute"; 
		leftover.style.visibility = "hidden"; 
		document.body.appendChild(leftover);
	}	
	if(topover == null)
	{
		topover = document.createElement("DIV");
		topover.id = "topover";
		topover.style.backgroundColor = "#fb8844";
		topover.style.height = "2px";
		topover.style.fontSize = "1px"; 
		topover.style.filter = "progid:DXImageTransform.Microsoft.Alpha(Opacity=25, FinishOpacity=25, Style=1, StartY=0, FinishY=0, StartX=0, FinishX=100)"; 
		topover.style.zIndex = 3001; 
		topover.style.display = "none"; 
		topover.style.position = "absolute";
		topover.style.visibility = "hidden"; 
		document.body.appendChild(topover); 
	}	
	if(rightover == null)
	{
		rightover = document.createElement("DIV");
		rightover.id = "rightover";
		rightover.style.backgroundColor = "#fb8844";
		rightover.style.width = "2px";
		rightover.style.position = "absolute";
		rightover.style.display = "none"; 
		rightover.style.visibility = "hidden"; 
		rightover.style.fontSize = "1px"; 
		rightover.style.filter = "progid:DXImageTransform.Microsoft.Alpha(Opacity=25, FinishOpacity=100, Style=1, StartY=0, FinishY=100, StartX=0, FinishX=0)"; 
		rightover.style.zIndex = 3001; 
		document.body.appendChild(rightover);
	}	
	if(bottomover == null)
	{
		bottomover = document.createElement("DIV");
		bottomover.id = "bottomover";
		bottomover.style.backgroundColor = "#fb8844";
		bottomover.style.height = "2px";
		bottomover.style.fontSize = "1px";
		bottomover.style.zIndex = 3001; 
		bottomover.style.display = "none"; 
		bottomover.style.position = "absolute";
		bottomover.style.visibility = "hidden"; 
		document.body.appendChild(bottomover); 
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
			line.style.width = ((index + 1) * 2) + "px";
			line.style.height = "1px";
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
			line.style.width = ((index + 1) * 2) + "px";
			line.style.height = "1px";
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
		arrow.style.width = "4px";
		arrow.style.height = "4px";
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
		arrow.style.height = "4px";
		arrow.style.width = "4px";
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
function showResizeLine(event)
{
	if(columnresizeMode == 1)	
		showColumnResizeLine(event);
	else if(columnresizeMode == 2)	
		showColumnSetResizeLine(event); 
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
function ShowColumnPressed(column, pressed)	
{	
	var _colpressed = document.getElementById("colpressed");	
	if(column.getAttribute("type") != null && column.getAttribute("type") == "header") 
	{
		_colpressed.style.borderLeft = column.style.borderLeft;
		_colpressed.style.borderRight = column.style.borderRight;
		_colpressed.style.borderTop = column.style.borderTop; 
		_colpressed.style.borderBottom = column.style.borderBottom; 		
	}
	else
	{					
		if(isDefaultView)
		{
			var _computedstyle = document.defaultView.getComputedStyle(column, null); 
			_colpressed.style.borderLeft = _computedstyle.getPropertyValue("border-left-width") + " " + _computedstyle.getPropertyValue("border-left-style") + " " + _computedstyle.getPropertyValue("border-left-color"); 
			_colpressed.style.borderRight = _computedstyle.getPropertyValue("border-right-width") + " " + _computedstyle.getPropertyValue("border-right-style") + " " + _computedstyle.getPropertyValue("border-right-color");
			_colpressed.style.borderTop = _computedstyle.getPropertyValue("border-top-width") + " " + _computedstyle.getPropertyValue("border-top-style") + " " + _computedstyle.getPropertyValue("border-top-color"); 
			_colpressed.style.borderBottom = _computedstyle.getPropertyValue("border-bottom-width") + " " + _computedstyle.getPropertyValue("border-bottom-style") + " " + _computedstyle.getPropertyValue("border-bottom-color");
		}
		else
		{
			_colpressed.style.borderLeft = column.style.borderLeft;
			_colpressed.style.borderRight = column.style.borderRight;
			_colpressed.style.borderTop = column.style.borderTop; 
			_colpressed.style.borderBottom = column.style.borderBottom; 
		}
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
	if(isDefaultView)
	{
		_colspan.style.borderLeftColor = document.defaultView.getComputedStyle(column.offsetParent, '').getPropertyValue("background-color"); 
		_colspan.style.borderRightColor = document.defaultView.getComputedStyle(column.offsetParent, '').getPropertyValue("background-color"); 
		_colspan.style.borderTopColor = document.defaultView.getComputedStyle(column.offsetParent, '').getPropertyValue("background-color"); 
		_colspan.style.borderBottomColor = document.defaultView.getComputedStyle(column.offsetParent, '').getPropertyValue("background-color"); 
	}
	else
	{
		_colspan.style.borderLeftColor = column.offsetParent.style.borderLeftColor;		
		_colspan.style.borderRightColor = column.offsetParent.style.borderRightColor;
		_colspan.style.borderTopColor = column.offsetParent.style.borderTopColor;
		_colspan.style.borderBottomColor = column.offsetParent.style.borderBottomColor;
	}
	currpressedcolumn = column; 
}	
function ShowColumnUnPressed()
{
	if(currpressedcolumn != null)
	{
		var _colpressed = document.getElementById("colpressed"); 
		currpressedcolumn.style.borderLeftColor = _colpressed.style.borderLeftColor;
		currpressedcolumn.style.borderRightColor = _colpressed.style.borderRightColor;
		currpressedcolumn.style.borderTopColor = _colpressed.style.borderTopColor;
		currpressedcolumn.style.borderBottomColor = _colpressed.style.borderBottomColor;		
		var _colspanpressed = _colpressed.childNodes[0]; 		
		var _colspan = _colpressed.childNodes[0]; 
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
	else if(gridex.offsetParent != null && gridex.offsetParent.tagName == "BODY")
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
function getColumnFromElement(element)
{	
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
			
	return element;
}
function getPixelValue(value)
{	
	if(value == null || value.length == 0)
		return 0; 
		
	var pixelvalue = 0; 	
	var indexof = value.indexOf("px"); 	
	if(indexof > 0)
		pixelvalue = parseInt(value.substr(0, indexof)); 
	else
	{
		try
		{
			pixelvalue = parseInt(value, 10); 
		}
		catch(err)
		{
			pixelvalue = 0; 
		}
	}
		
	return pixelvalue; 
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
	return getPixelValue(width); 	
}
function getPixelColWidth(width)
{
	return getPixelValue(width); 	
}
function getPadding(padding)
{
	return getPixelValue(padding);
}
function getPaddingBottom(item)
{
	var _style = null;
	_style = item.style; 
	if(_style.paddingBottom != "")
		return getPadding(_style.paddingBottom);	
		
	if(isDefaultView)
	{
		_style = document.defaultView.getComputedStyle(item, null); 
		if(_style.getPropertyValue("padding-bottom") != "")
			return getPadding(_style.getPropertyValue("padding-bottom"));
	}		
	return 0;
}
function getPaddingTop(item)
{
	if(item == null)
		return; 
		
	var _style = null;
	_style = item.style; 
	if(_style.paddingTop != "")
		return getPadding(_style.paddingTop);	
	
	if(isDefaultView)
	{	
		_style = document.defaultView.getComputedStyle(item, null);
		if(_style.getPropertyValue("padding-top") != "")
			return getPadding(_style.getPropertyValue("padding-top"));
	}		
	return 0;	
}
function getPaddingLeft(item)
{	
	if(item == null)
		return 0;
		
	var _style = null;
	_style = item.style; 
	if(_style.paddingLeft != "")
		return getPadding(_style.paddingLeft);	
	
	if(isDefaultView)
	{
		_style = document.defaultView.getComputedStyle(item, null); 
		if(_style.getPropertyValue("padding-left") != "")
			return getPadding(_style.getPropertyValue("padding-left"));
	}		
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
		
	if(isDefaultView)
	{
		_style = document.defaultView.getComputedStyle(item, null); 
		if(_style.getPropertyValue("padding-right") != "")
			return getPadding(_style.getPropertyValue("padding-right")); 		
	}
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
	else if(td.defaultView.borderTopWidth != "")
		width = getBorderStyleWidth(td.defaultView.borderTopWidth); 
	return width; 	
}
function getBorderBottomWidth(td)
{
	var width = 0; 
	if(td.style.borderBottomWidth != "")
		width = getBorderStyleWidth(td.style.borderBottomWidth);
	else if(td.defaultView.borderBottomWidth != "")
		width = getBorderStyleWidth(td.defaultView.borderBottomWidth); 
	return width; 
}
function getBorderLeftWidth(td)
{
	var width = 0;
	if(td.style.borderLeftWidth  != "")
		width = getBorderStyleWidth(td.style.borderLeftWidth); 
	else if(td.defaultView.borderLeftWidth != "")
		width = getBorderStyleWidth(td.defaultView.borderLeftWidth);
	return width;
}
function getBorderRightWidth(td)
{
	var width = 0; 
	if(td.style.borderRightWidth != "")
		width = getBorderStyleWidth(td.style.borderRightWidth);
	else if(td.defaultView.borderRightWidth != "")
		width = getBorderStyleWidth(td.defaultView.borderRightWidth); 
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
		width += getPixelValue(td.style.borderLeftWidth); 		 	
	else if(isDefaultView && document.defaultView.getComputedStyle(td, null).getPropertyValue("border-left-width") != "")
	{
		var _borderwidth = document.defaultView.getComputedStyle(td, null).getPropertyValue("border-left-width");
		width += getPixelValue(_borderwidth); 		
	}	
	if(td.style.borderRightWidth != "")
		width += getPixelValue(td.style.borderRightWidth); 		
	else if(isDefaultView && document.defaultView.getComputedStyle(td, null).getPropertyValue("border-right-width") != "")
	{
		var _borderwidth = document.defaultView.getComputedStyle(td, null).getPropertyValue("border-right-width"); 
		width += getPixelValue(_borderwidth); 		
	}	
	return width; 
}
function getInnerSpan(element)
{
	var _length = element.childNodes.length; 
	for(var ichild = 0; ichild < _length; ichild++)
	{
		if(element.childNodes[ichild].tagName == "SPAN")
			return element.childNodes[ichild]; 			
	}
	throw Error("invalid inner SPAN"); 
}
function isInResizeArea(td, htmlTable)
{
	var x = window.event.offsetX;
	var y = window.event.offsetY; 
	var ylow = 0;
	var yhigh = ylow + td.offsetHeight; 		
	var xlow;
	var xhigh; 				
	if(td.offsetLeft + td.offsetWidth + 2 <= htmlTable.offsetWidth)
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
function getLineHeight(slineheight)
{
	var lineheight = 1.2;
	var indexof = -1;
	
	indexof = slineheight.indexOf("px");
	if(indexof > 0)
		return parseInt(slineheight.substring(0, indexof), 10);
		
	indexof = slineheight.indexOf("pt");
	if(indexof > 0)
		return parseInt(slineheight.substring(0, indexof), 10);
		
	return 1.2; 
}
function getFontSize(sfontsize)
{	
	var indexof = -1; 	
	indexof = sfontsize.indexOf("px"); 
	if(indexof > 0)
		return parseInt(sfontsize.substring(0, indexof), 10);
	
	indexof = sfontsize.indexOf("pt"); 
	if(indexof > 0)
		return parseInt(sfontsize.substring(0, indexof), 10); 	
	
	return 12; 
}
function fontSizeInPoints(sfontsize)
{
	var inpoints = false;
	if(sfontsize.indexOf("pt")) inpoints = true;	
	return inpoints;
}
function setCellMaxLines(td, span)
{
	var lineheight = 1.2;
	if(span.currentStyle.lineHeight != "normal")	
		lineheight = getLineHeight(span.currentStyle.lineHeight); 	
	
	var maxlines = parseInt(td.maxlines, 10);	
	var fontsize = getFontSize(span.currentStyle.fontSize);
	var pixelfactor = 1;
	if(fontSizeInPoints(span.currentStyle.fontSize)) pixelfactor = 1.3; 
	
	span.style.pixelHeight = Math.round(lineheight * fontsize * maxlines * pixelfactor);	
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
		if(element.tagName == "BODY" && element.style.overflow == "hidden")
		{
			//if(rv == null || rv >= 1.6)
			//	top += getTopOffset(element);
		}
		element = element.offsetParent; 		
	}	
	return top; 
}
function getBottomOffset(element)
{
	var offset = 0; 
	if(isDefaultView)
		offset += getPixelWidth(document.defaultView.getComputedStyle(element, null).getPropertyValue("margin-bottom")); 
	else
		offset += getPixelWidth(element.style.marginBottom); 
	return offset; 
}
function getTopOffset(element)
{
	var offset = 0; 
	if(isDefaultView)
		offset += getPixelWidth(document.defaultView.getComputedStyle(element, null).getPropertyValue("margin-top")); 
	else
		offset += getPixelWidth(element.style.marginTop);
	return offset; 	
}
function getOffsetTopForEdit(gridEXHtml)
{
	var offset = 0; 	
	var _style = null; 
	if(isDefaultView)
		_style = document.defaultView.getComputedStyle(gridEXHtml, null); 
	if((_style == null && gridEXHtml.style.position == "relative") || (_style != null && _style.getPropertyValue("position") == "relative"))
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
	var _style = null; 
	if(isDefaultView)
		_style = document.defaultView.getComputedStyle(gridEXHtml, null); 
	if((_style == null && gridEXHtml.style.position == "relative") || (_style != null && _style.getPropertyValue("position") == "relative"))
	{
		var _offsetParent = getGridEXOffsetParent(gridEXHtml); 
		if(_offsetParent.tagName != "BODY")
			offset += getPixelLeft(_offsetParent); 
	}
	return offset; 
}
function getPixelLeft(element)
{
	var left = 0; 
	while(element != null)
	{		
		/*	
		if(rv >= 1.6)		
		{			
			if(element.style.overflow != "hidden")
				left += element.offsetLeft; 
		}
		else
		*/
			left += element.offsetLeft;		
		if(element.tagName == "BODY" && element.style.overflow == "hidden")
		{			
			//if(rv == null || rv >= 1.6)
			//	left += getPixelWidth(document.defaultView.getComputedStyle(element, null).getPropertyValue("margin-left"));
		}
		element = element.offsetParent; 
	}
	return left; 
}
function getRootRowFromInner(element)
{
	while(element != null)
	{			
		if(element.nodeType == 1 && element.tagName == "TR" && element.id != null && element.getAttribute("t") != null)
			return element; 
		
		element = element.parentNode; 
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
	
		if(element.tagName == "TR" && element.id != null && element.getAttribute("t") != null) // element.type != null && parseInt(element.type, 10) == 1
			isrow = true;
		else
			element = element.parentNode; 		
	} while(!isrow); 			
	return element; 	
}
function getHierarchicalRowTop(element)
{	
	element = getHierarchicalRow(element); 		
	return element.offsetTop;	
}
function getSortWidth(column)
{			
	if(column.childNodes.length == 2)
		return column.childNodes[1].offsetWidth; 
	else
		return 0; 
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
	var _length = _colsdef.length; 	
	if(_length > 0)
	{		
		var _coldef = null; 	
		var _change = false; 
		for(var i = 0; i < _length; i++)
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
	var input = document.getElementsByName(gridexID + "_eventdata")[0]; 
	if(input == null)
		throw Error("event data field is null"); 
			
	input.value = gaction + ":" + gtable + ":" + gpos + ":" + gnewpos + ":" + gcolumn + ":" + hcolumnpos; 
}