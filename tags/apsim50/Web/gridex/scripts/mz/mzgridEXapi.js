//////////////////////////////////////////////////////////////////
// GridEX JavaScript MZ API 1.1.1009
// Copyright by Janus Systems S.A.
// 2002 - 2004
//////////////////////////////////////////////////////////////////
function GridEXGroupByBox(div, gridex)
{	
	var type = 0; 
	var columns = null; 
	var gridEX = gridex;
	var htmlBox = div.getElementsByTagName("TABLE")[0];		
	this.getGridEX = getGridEX;
	this.getHeight = getHeight; 
	this.getHtmlBox = getHtmlBox;	
	this.DropColumn = DropColumn;
	this.HitTestColumns = HitTestColumns; 
	this.ShowColumnForDrop = ShowColumnForDrop;	
	function getGridEX() { return gridEX; }	
	function getHtmlBox() { return htmlBox; }	
	function getHeight()
	{
		if(htmlBox == null)
			return 0;
			
		return htmlBox.parentElement.offsetHeight; 
	}	
	function HitTestColumns(x, y, testmode)
	{					
		var cell = null; 
		var row = null;	
		var xlow = -1;
		var ylow = -1;
		var xhigh = -1;
		var yhigh = -1; 		
		var xcelllow = null;
		var xcellhigh = null; 
		var ycelllow = null;
		var ycellhigh = null; 		
		var table = null; 		
		var _colslength = -1; 
		var _rowslength = -1;		
		if(currcolumn != null && currcolumn.getAttribute("type") == "header")
			return null;
			
		xlow = htmlBox.offsetLeft + gridEX.getPixelLeft();
		xhigh = xlow + htmlBox.offsetWidth;		
		ylow = htmlBox.offsetTop + gridEX.getPixelTop();
		yhigh = ylow + htmlBox.offsetHeight; 		
		if(y >= ylow && y <= yhigh)
		{		
			if(testmode == 1) 
			{	
				if(type == 1) 
				{			
					if(columns != null && columns.length > 0)
					{	
						cell = null; 
						_colslength = columns.length; 
						for(var index = 0; index < _colslength; index = index + 3)
						{						
							if(currheader.getGridEXTable().getID() == columns[index+2])
							{
								cell = columns[index+1];
								xcelllow = getPixelLeft(cell);	
								xcellhigh = xcelllow + cell.offsetWidth; 								
								ycelllow =  getPixelTop(cell); 
								ycellhigh = ycelllow + cell.offsetHeight;								
								if((x >= xcelllow && x <= xcellhigh) && (y >= ycelllow && y <= ycellhigh))
									return [cell, columns[index+2], 0]; 
							}
						}						
						_rowslength = htmlBox.rows.length; 						
						row = null; 
						table = null; 
						for(var index = 0; index < _rowslength; index++)
						{
							row = htmlBox.rows[index]; 
							if((currheader.getGridEXTable().getID() == row.getAttribute("id")) || (currheader.getGridEXTable().getParent() != null && currheader.getGridEXTable().getParent().getID() == row.getAttribute("id")))
							{						
								if(y >= (ylow + row.offsetTop) && y <= (ylow + row.offsetTop + row.offsetHeight))
								{								
									table = row.cells[0].childNodes[0];									
									if(table.cells != null)
										cell = table.cells[table.cells.length-1]; 
									else
										cell = getCellsCore(table)[getCellsCore(table).length-1]; 
									if(x >= getPixelLeft(cell) + cell.offsetWidth)	
									{	
										if(currheader.getGridEXTable().getID() == row.id)
										{
											if(table.cells = null)
												return [table.cells[table.cells.length-1], row.id, 1]; 
											else
												return [getCellsCore(table)[getCellsCore(table).length-1],row.id, 1]; 
										}
										else if(currheader.getGridEXTable().getParent() != null && currheader.getGridEXTable().getParent().getID() == row.id)
										{
											if(!groupsInTable(currheader.getGridEXTable().getID()))
											{
												if(table.cells != null)
													return [table.cells[table.cells.length-1], currheader.getGridEXTable().getID(), -1];
												else
													return [getCellsCore(table)[getCellsCore(table).length-1],currheader.getGridEXTable().getID(), -1];
											}
										}
									}
								}
							}
						}						
					}					
				}
				else if(type == 0)
				{				
					if(htmlBox.cells != null)
						return [htmlBox.cells[htmlBox.cells.length-1], currheader.getGridEXTable().getID(), -1]; 
					else
					{
						var x = getCellsCore(htmlBox);	
						return [x[x.length-1],currheader.getGridEXTable().getID(), -1]; 
					}
				}
			}			
			else if(testmode == 2) 
			{					
				_colslength = columns.length; 
				for(var icolumn = 0; icolumn < _colslength; icolumn = icolumn + 3)
				{
					if(columns[icolumn + 2] == currgrouptable)
					{
						cell = columns[icolumn + 1]; 
						xcelllow = getPixelLeft(cell); 
						xcellhigh = xcelllow + cell.offsetWidth;						
						ycelllow = getPixelTop(cell); 
						ycellhigh = ycelllow + cell.offsetHeight; 						
						if((x >= xcelllow && x <= xcellhigh) && (y >= ycelllow && y <= ycellhigh))
							return [cell, currgrouptable];						
					}					
				}
			}
		}		
		return null;
	}	
	function ShowColumnForDrop(x, column, table, ordershow)	
	{			
		var xpos = -1;
		var ylow = -1;
		var yhigh = -1;		
		var xcellLow = -1;
		if(ordershow == 0)
		{			
			if(columndragMode == 3)
			{
				if(column.getAttribute("name") == currcolumn.getAttribute("name"))
					return; 
		
				xpos = getPixelLeft(column); 					
				ylow = getPixelTop(column); 
				yhigh = ylow + column.offsetHeight; 					
				var currpos = parseInt(currcolumn.getAttribute("name"), 10);
				var newpos = parseInt(column.getAttribute("name"), 10); 											
				xcellLow = xpos; 				
				if(newpos < currpos && newpos + 1 <= currpos && (x >= xcellLow && x <= xcellLow + (column.offsetWidth / 2)))
					groupnewpos = newpos;
				else if(newpos < currpos && newpos + 1 < currpos && (x > xcellLow + (column.offsetWidth / 2) && x <= xcellLow + column.offsetWidth))
				{
					xpos += column.offsetWidth; 
					groupnewpos = newpos;
				}
				else if(newpos > currpos && newpos - 1 > currpos && (x >= xcellLow && x <= xcellLow + (column.offsetWidth / 2)))					
					groupnewpos = newpos;
				else if(newpos > currpos && newpos - 1 >= currpos && (x > xcellLow + (column.offsetWidth / 2) && x <= xcellLow + column.offsetWidth))
				{
					xpos += column.offsetWidth; 
					groupnewpos = newpos;
				}
				else
					return;
			}
			else		
			{	
				xpos = getPixelLeft(column); 					
				ylow = getPixelTop(column); 
				yhigh = ylow + column.offsetHeight; 				
				xcellLow = xpos;
				if(x >=  xcellLow && x <= xcellLow + (column.offsetWidth / 2))				
					groupnewpos = parseInt(column.getAttribute("name"), 10);
				else if(x > xcellLow + (column.offsetWidth / 2) && x <= xcellLow + column.offsetWidth)
				{								
					xpos += column.offsetWidth; 
					groupnewpos = parseInt(column.getAttribute("name"), 10) + 1;	
				}
				else
					return; 
			}
		}
		else if(ordershow ==1)
		{
			xpos = getPixelLeft(column); 					
			ylow = getPixelTop(column); 
			yhigh = ylow + column.offsetHeight; 		
			xpos += column.offsetWidth;			
			if(type == 1)
			{	
				var _length = htmlBox.rows.length; 
				var row = null;
				for(var index = 0; index < _length; index++)
				{
					row = htmlBox.rows[index]; 
					if(row.id == table) 
					{
						var _table = row.cells[0].childNodes[0];
						if(_table.cells != null)
							cell = _table.cells[_table.cells.length-1];
						else
							cell = getCellsCore(_table)[getCellsCore(_table).length-1]; 
						if(cell.getAttribute("name") == null || cell.getAttribute("name") == "")
							groupnewpos = 0; 
						else
							groupnewpos = parseInt(cell.getAttribute("name"), 10) + 1;						
						break; 
					}
				}
			}
			else if(type == 0)
				groupnewpos = 0; 				
		}
		else if(ordershow == -1)
		{		
			xpos = getPixelLeft(column);
			ylow = getPixelTop(column); 
			yhigh = ylow + column.offsetHeight;			
			xpos += column.offsetWidth;
			groupnewpos = 0;						
		}
		currgrouptable = table;		
		drawUpArrow(xpos, ylow);
		drawDownArrow(xpos, yhigh);
	}	
	function DropColumn()
	{	
		var action = -1;
		if(columndragMode == 3)
		{
			if(droptarget == -1)
				action = 2;
			else
			{
				if(parseInt(currcolumn.getAttribute("name"), 10) != currgrouppos)
					action = 3;
			}
		}
		else
			action = 1;
			
		var cancel = getGridEX().FireEvent("GroupsChanging", [getGridEX().getTables().getTableByID(currgrouptable).getGridEXColumnByClientID(currcolumn.id), action]);
		if(cancel == null || !cancel)
		{
			if(columndragMode == 3)
			{	
				if(droptarget == -1)
					setGroupEventData(getGridEX().getID(), action, currgrouptable, currcolumn.getAttribute("name"), "null", "null", "null"); 
				else
				{				
					if(parseInt(currcolumn.name, 10) != currgrouppos)
						setGroupEventData(getGridEX().getID(), action, currgrouptable, currcolumn.getAttribute("name"), groupnewpos, "null", "null"); 				
					else
						return; 
				}
			}
			else
				setGroupEventData(getGridEX().getID(), action, currgrouptable, "null", groupnewpos, currcolumn.id, "null");
		
			getGridEX().DoPostBack(null, "GroupsChanging");
		}
		else
			ShowColumnUnPressed();
			
		endColumnDrag();
	}	
	function column_onmousedown()
	{				
		couldStartDrag = true;
		dragpoint = new Point(window.event.clientX, window.event.clientY); 
	}
	function column_onmousemove()
	{
		if(couldStartDrag && dragpoint != null)
		{
			if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
			{	
				var groupcolumn = getColumnFromElement(window.event.srcElement); 
				startGroupDrag(groupcolumn, groupByBox, getGroupTable(groupcolumn.id));
				couldStartDrag = false;
				dragpoint = null; 
			}
		}
	}	
	function column_onmouseup()
	{		
		if(!columnDraging)
		{
			couldStartDrag = false; 
			dragpoint = null; 
			var _column = getColumnFromElement(window.event.srcElement); 			
			var _input = document.getElementsByName(getGridEX().getID() + "_eventdata")[0]; 
			if(_input == null)
				throw Error("unable to find event data"); 
			
			_input.value = getGroupTable(_column.id) + ":" + _column.getAttribute("name"); 						
			getGridEX().DoPostBack(null, "GroupByBoxHeaderClick"); 						
		}
	}		
	function groupsInTable(tableID)
	{
		if(columns != null)
		{
			var l = columns.length; 
			for(var i = 0; i < l; i = i + 3)
			{
				if(columns[i+2] == tableID)
					return true; 
			}
		}		
		return false; 
	}	
	function getGroupTable(columnID)
	{				
		if(columns != null)
		{
			var l = columns.length; 
			for(var i = 0; i < l; i = i + 3)
			{				
				if(columns[i] == columnID)
					return columns[i+2];
			}
		}		
		throw new Error("argument out of range exception"); 
	}		
	if(htmlBox != null)
	{		
		if(htmlBox.getAttribute("type") != null)
		{						
			var cell = null;
			var length = -1;
			var row = null;
			var table = null;
			columns = new Array(); 			
			var _rowslength = htmlBox.rows.length; 
			var _cellslength = -1; 
			for(var irow = 0; irow < _rowslength; irow++)
			{				
				row = htmlBox.rows[irow];
				table = row.cells[0].childNodes[0]; 
				var _tablecells = null;
				if(table.cells != null)
					_tablecells = table.cells;
				else
					_tablecells = table.getElementsByTagName("TD"); 
				_cellslength = _tablecells.length; 
				for(var icell = 0; icell < _cellslength; icell++)
				{
					cell = _tablecells[icell];
					if(cell.getAttribute("type") != null && parseInt(cell.getAttribute("type"), 10) == 1)
					{
						length = columns.length; 
						columns[length] = cell.getAttribute("id"); 
						columns[length + 1] = cell; 
						columns[length + 2] = row.getAttribute("id"); 
						cell.addEventListener("mousedown", column_onmousedown, false); 
						cell.addEventListener("mouseup", column_onmouseup, false); 
						cell.addEventListener("mousemove", column_onmousemove, false); 
					}
				}
			}
			type = 1; 
		}
		else
			type = 0; 
	}	
	var groupByBox = this; 
	return this;
}
function GridEXSelectedItem(row)
{	
	var gridEXRow = row; 			
	this.getRow = getRow; 	
	function getRow() { return gridEXRow; }		
	return this; 
}
function GridEXSelectedItemCollection(gridex, selectedItems)
{	
	var gridEX = gridex; 	
	var arrSelectedItems = new Array();	
	if(selectedItems != null && selectedItems.length > 0)
	{	
		var row = null; 		
		var l = selectedItems.length;
		for(var i = 0; i < l; i++)		
		{			
			row = document.getElementById(selectedItems[i]);
			if(row == null)
				throw Error("row '" + selectedItems[i] + "' is null or invalid for selected item"); 									
			
			arrSelectedItems[arrSelectedItems.length] = new GridEXSelectedItem(gridEX.RetrieveRow(row, getInnerItemRow(row) ,null)); 						
		}
		gridEX.ReportRowsStatus(); 
	}	
	this.getSelectedItemInIndex = getSelectedItemInIndex;
	this.Clear = Clear;
	this.Count = Count; 	
	this.IsRowSelected = IsRowSelected;
	this.SelectRow = SelectRow;
	this.SelectSingleRow = SelectSingleRow; 	
	function getSelectedItemInIndex(index)
	{		
		if(index < 0 || index >= Count())
			throw Error("argument out of range");
			
		return arrSelectedItems[index]; 
	}
	function UnSelectRow(row)
	{		
		var innerRow = row.getInnerRow();
		innerRow.className = getClassName(row);			
		var innerPreviewRow = row.getPreviewInnerRow(); 
		if(innerPreviewRow != null)
			innerPreviewRow.className = getPreviewClassName(row); 
	}
	function Clear()
	{		
		var l = arrSelectedItems.length;
		for(var i = 0; i < l; i++)
			UnSelectRow(arrSelectedItems[i].getRow());
			
		arrSelectedItems.length = 0; 
	}
	function Count() { return arrSelectedItems.length; }		
	function IsRowSelected(row)
	{
		if(arrSelectedItems.length == 0)
			return false; 
			
		var _row= null; 
		var l = arrSelectedItems.length; 
		for(var i = 0; i < l; i++)
		{
			_row = arrSelectedItems[i].getRow(); 
			if(_row.getID() == row.getID() && _row.getTable().getID() == row.getTable().getID())
				return true; 
		}		
		return false; 
	}
	function SelectRow(row)
	{
		if(gridEX.getSelectionMode() == 1)
			SelectSingleRow(row);
		else if(gridEX.getSelectionMode() == 2)
			SelectMultipleRow(row); 
	}
	function SelectSingleRow(rowToSelect)
	{		
		if(getGridEX().getSelectionMode() != 1 && Count() == 1 && IsRowSelected(rowToSelect))
			return;
			
		if(IsRowSelected(rowToSelect) && getGridEX().getSelectionMode() == 1)
			return; 
			
		ResetLastRow(); 
		Clear(); 
		Add(rowToSelect);		
		getGridEX().ReportRowsStatus();
		if(getGridEX().selpb)			
			getGridEX().DoPostBack(null, "SelectionChanged:"+rowToSelect.getID()); 
	}
	var lastrow = null; 
	var focusrow = null; 
	function ResetLastRow()
	{
		if(focusrow != null)
		{
			var innerRow = focusrow.getInnerRow();
			innerRow.className = getClassName(focusrow); 
		}
		lastrow = focusrow = null; 
	}
	function SelectMultipleRow(row)
	{
		if(window.event.ctrlKey)
		{			
			if(window.event.type == "click")
			{	
				ResetLastRow();		
				Add(row);
			}
			else if(window.event.type == "keydown")
			{
				var innerRow = null;
				if(focusrow != null)
				{
					innerRow = focusrow.getInnerRow(); 
					innerRow.className = getClassName(row); 
				}
				innerRow = row.getInnerRow();
				innerRow.className += " " + getGridEX().focusRowCss;
				lastrow = focusrow = row;
			}
			else
				ResetLastRow();
				
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
		}
		else if(window.event.shiftKey)
		{
			if(Count() == 0)
				Add(row);
			else
			{
				if(lastrow == null)
					lastrow = arrSelectedItems[arrSelectedItems.length-1].getRow();				
				if(row.getPosition() > lastrow.getPosition())
				{
					var b = false; 
					for(var i=0;i<arrSelectedItems.length;i++)
					{
						var r = arrSelectedItems[i].getRow(); 
						if(r.getPosition() < lastrow.getPosition() || r.getPosition() > row.getPosition())
						{
							UnSelectRow(r);
							arrSelectedItems[i] = null; 
							b= true;
						}						
					}
					if(b) rebuildArray(arrSelectedItems); 
					for(var i=lastrow.getPosition();i<=row.getPosition();i++)
						Add(getGridEX().getRow(i));
				}
				else
				{
					var b = false;
					for(var i=0;i<arrSelectedItems.length;i++)
					{
						var r=arrSelectedItems[i].getRow();
						if(r.getPosition() > lastrow.getPosition() || r.getPosition() < row.getPosition())
						{
							UnSelectRow(r);
							arrSelectedItems[i] = null; 
							b = true;
						}
					}
					if(b) rebuildArray(arrSelectedItems); 
					for(var i=lastrow.getPosition(); i>=row.getPosition(); i--)
						Add(getGridEX().getRow(i)); 
				}			
			}
		}		
		else
		{
			ResetLastRow();
			Clear(); 
			Add(row); 
		}
		getGridEX().ReportRowsStatus(); 
	}			
	function getGridEX() { return gridEX; }	
	function getInnerItemRow(row)
	{
		var _innerItemRow = null; 
		var _tableID = null; 
		if(getGridEX().isHierarchicalGrid())
		{			
			var _tmpTable = row.cells[0].childNodes[0]; 
			var _tmpRow = _tmpTable.rows[0]; 
			var _tmpCellsLength = _tmpRow.cells.length; 
			for(var _tmpCell = 0; _tmpCell < _tmpCellsLength && _innerItemRow == null; _tmpCell++)
			{
				if(_tmpRow.cells[_tmpCell].childNodes[0].tagName == "TABLE")
					_innerItemRow = _tmpRow.cells[_tmpCell].childNodes[0].rows[0]; 
			}			
			if(_innerItemRow == null)			
				throw Error("unable to retrieve inner row"); 
		}		
		if(row.getAttribute("t") != null)
			_tableID = row.getAttribute("t"); 
		if(_tableID != null && getGridEX().getTables().getTableByID(_tableID).getUseColumnSets())
		{
			if(_innerItemRow == null)
				_innerItemRow = row;
			
			if(_innerItemRow.getAttribute("type") == null || _innerItemRow.getAttribute("type") == "3" || _innerItemRow.getAttribute("type") == "4" || _innerItemRow.getAttribute("type") == "9")
				return _innerItemRow.cells[0].childNodes[0].rows[0]; 
			else
				return _innerItemRow; 
		}
		else if(getGridEX().isHierarchicalGrid() && _innerItemRow != null)
			return _innerItemRow;
		else
			return row;
	}			
	function Add(row)
	{		
		if(getGridEX().getSelectionMode() == 3)
			return; 
			
		var innerPreviewRow = null; 		
		var innerRow = row.getInnerRow(); 
		innerRow.className = getSelectedClassName(row);		
		var innerPreviewRow = row.getPreviewInnerRow(); 
		if(innerPreviewRow !=null)
			innerPreviewRow.className = getPreviewSelectedClassName(row); 

		var item = new GridEXSelectedItem(row);
		arrSelectedItems[arrSelectedItems.length] = item; 
	}	
	function Clear()
	{
		var innerRow = null; 
		var innerPreviewRow = null; 
		var row = null;		
		var l = arrSelectedItems.length;
		for(var i = 0; i < l ; i++)
		{
			row = arrSelectedItems[i].getRow();
			innerRow = row.getInnerRow();
			innerRow.className = getClassName(row);			
			innerPreviewRow = row.getPreviewInnerRow(); 
			if(innerPreviewRow != null)
				innerPreviewRow.className = getPreviewClassName(row); 
		}		
		arrSelectedItems.length = 0; 
	}					
	return this; 
}
function GridEXColumnHeaders(gridEXTable, htmlRow, htmlTable, headerType, headerIndex, isRoot)
{		
	if(htmlTable == null)
		throw Error("invalid htmlTable for column headers"); 

	var autoResized = false; 

	var headerindex = -1; 
	if(headerIndex != null)
		headerindex = headerIndex; 
		
	var isroot = (isRoot != null) ? isRoot : false;
	var columnsets = null; 
	var gridEXTable = gridEXTable;				
	var haveColumnSets = false; 
	var headerType = headerType;
	var htmlTable = htmlTable;	
	var htmlRootRow = htmlRow;	
	this.getColumnSets = getColumnSets; 
	this.getHtmlHeader = getHtmlHeader; 
	this.getGridEXTable = getGridEXTable; 
	this.getGridEX = getGridEX; 
	this.getIndex = getIndex; 
	this.getIsAutoSized = getIsAutoSized;
	this.getIsRoot = getIsRoot;
	this.getIsVisible = getIsVisible; 
	this.getHtmlColumnById = getHtmlColumnById;
	this.getRowIndex = getRowIndex; 
	this.setColumnSets = setColumnSets; 	
	this.AutoSizeColumn = AutoSizeColumn; 
	this.AutoSizeColumns = AutoSizeColumns; 
	this.AutoSizeColumnsAfterDisplay = AutoSizeColumnsAfterDisplay;
	this.CheckSelectors = CheckSelectors;
	this.ColumnAutoSize = ColumnAutoSize; 
	this.DropColumn = DropColumn;
	this.HitTestColumns = HitTestColumns; 	
	this.ResizeColumnWidth = ResizeColumnWidth;
	this.ShowColumnForDrop = ShowColumnForDrop; 	
	var cellsresize = 0; 	
	function getColumnSets() { return columnsets; }	
	function getGridEXTable() { return gridEXTable; }	
	function getGridEX() { return gridEXTable.getGridEX(); }	
	function getHtmlHeader() { return htmlTable; }	
	function getHtmlColumnById(id)
	{		
		if(headerType == 1) 
		{			
			var r = htmlTable.rows[0]; 
			var c = null;
			var j = r.cells.length; 
			for(var i = 0; i < j; i++)
			{
				c = r.cells[i]; 
				if(c.id == id) 
					return c; 
			}			
			throw new Error("argument out of range"); 
		}
		else 
		{
			var c = null; 
			var cs = null; 
			var j = columnsets.getCount(); 
			for(var i = 0; i < j; i++)
			{				
				cs = columnsets.getColumnSetInIndex(i); 
				c = cs.getHtmlColumnByID(id); 
				if(c != null)
					return c;
			}
			throw Error("argument out of range"); 
		}
	}	
	function getIndex() { return headerindex; }	
	function getIsAutoSized() { return autoResized; }	
	function getIsRoot() { return isroot; }		
	function getRowIndex()
	{
		if(htmlRootRow == null)
			return -1; 
		else
			return htmlRootRow.rowIndex; 
	}	
	function getIsVisible()
	{
		if(htmlRootRow != null)
			return (htmlRootRow.style.display != "none");
		else if(getIsRoot())
			return true;
		else
			return false;
	}
	function setColumnSets(columnSets)
	{
		columnsets = columnSets; 	
		haveColumnSets = (columnsets != null); 
	}	
	function swapColumnHeader(colX, colY)
	{		
		var _cell = null;
		_cell = colY.childNodes[0]; 				
		_cell.style.pixelWidth = (colY.offsetWidth + getGridEXTable().getHeaderWidth() - (getPaddingLeft(colY) + getPaddingRight(colY) + getBorderWidth(colY) + getSortWidth(colY))); // + getGridEXTable().getHeaderWidth() 		
		if(colY.getAttribute("pec") != null && colY.getAttribute("type") != "ch")
			__cell.style.width = (getPixelValue(_cell.style.width) + 18) + "px";
		
		_cell = colX.childNodes[0]; 
		_cell.style.pixelWidth = (colX.offsetWidth - getGridEXTable().getHeaderWidth() - (getPaddingLeft(colX) + getPaddingRight(colX) + getBorderWidth(colX) + getSortWidth(colX))); // - getGridEXTable().getHeaderWidth() 
		if(colX.getAttribute("pec") != null && colX.getAttribute("type") != "ch")
			_cell.style.width = (getPixelValue(_cell.style.width) + 18) + "px";
			
		colY.setAttribute("type","ch");
		if(colX.getAttribute("pec") != null)
			colY.setAttribute("pec", colX.getAttribute("pec")); 
		
		colX.setAttribute("type",null);
		colX.removeAttribute("pec");			
	}	
	function AutoSizeColumnsWithHeader(_header)
	{
		if(headerType == 0 && columnsets != null)
			columnsets.AutoSizeColumnsByHeader(_header); 
		else	
			AutoSizeColumnsByHeader(_header); 			
	}	
	function AutoSizeColumnsByHeader(_header)
	{	
		var cell = null; 
		var _cell = null;		
		var _htmlTable = _header.getHtmlHeader();
		var length = -1;
		if(_htmlTable.cells != null)
			length = _htmlTable.cells.length;
		else
			length = getCellsCore(_htmlTable).length;
		var cellswidth = new Array();
		for(var i=0; i<length; i++)
		{
			if(_htmlTable.cells != null)
				cell = _htmlTable.cells[i];
			else
				cell = getCellsCore(_htmlTable)[i];
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)							
				cellswidth[cellswidth.length] = cell.offsetWidth; 
		}						
		var igcell = 0;
		var diff = 0;
		var cellsize = 0;		
		for(var icell = 0; icell < length; icell++)
		{	
			if(_htmlTable.cells != null)
				cell = _htmlTable.cells[icell];
			else
				cell = getCellsCore(_htmlTable)[icell];
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null) 
			{				
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
				cellsize = cellswidth[igcell];
				if(_htmlTable.cells != null)
					_cell = htmlTable.cells[icell]; 
				else
					_cell = getCellsCore(htmlTable)[icell];
				if(cell.getAttribute("type") == "ch" && _cell.getAttribute("type") != "ch")
				{
					var _awidth = (cellsize - getGridEXTable().getHeaderWidth() - diff);
					_cell.childNodes[0].style.width = (_awidth) + "px";
					_cell.childNodes[0].getElementsByTagName("SPAN")[0].style.width = (_awidth) + "px";  
					if(_cell.getAttribute("pec") != null && _cell.getAttribute("type") != "ch")
					{
						_cell.childNodes[0].style.width = (_awidth +  18) + "px"; 
						_cell.childNodes[0].getElementsByTagName("SPAN")[0].style.width = (_awidth) + "px";
					}
				}
				else 
				{
					_cell.childNodes[0].style.width = (cellsize - diff) + "px";
					_cell.childNodes[0].getElementsByTagName("SPAN")[0].style.width = (cellsize - diff) + "px"; 										
				}				
				igcell++; 
			}
		}
	}	
	function AutoSizeColumn(column, width)	
	{
		if(headerType == 1)
		{	
			var _width = -1;
			var _cols = htmlTable.getElementsByTagName("COL"); 
			var _colswidth = null; 
			var diff = getPaddingLeft(column) + getPaddingRight(column) + getBorderWidth(column) + getSortWidth(column); 
			var _headers = getGridEXTable().getHeaders(); 
			var _oldcolumnwidth = -1;
			var _oldremainwidth = -1; 
			var _oldwidth = -1; 
			var _newremainwidth = -1; 
			var inallheaders = _headers.length > 1; 
			if(getGridEX().getColumnAutoResize())
			{
				_oldcolumnwidth = column.offsetWidth; 
				_oldwidth = htmlTable.offsetWidth; 
				_oldremainwidth = _oldwidth - _oldcolumnwidth; 			
			}
			_width = width - diff; 
			column.childNodes[0].style.width = width + "px"; 
			getInnerSpan(column.childNodes[0]).style.width = _width + "px"; 
			_cols[column.cellIndex].width = width + "px"; 
			column.style.width = width + "px";
			if(inallheaders)
				ResizeColumnInHeaders(column, _width, width); 
				
			if(getGridEX().getColumnAutoResize())
			{
				_newremainwidth = _oldwidth - width; 
				ResizeColumnsExcept(column.cellIndex, _oldremainwidth , _newremainwidth, _cols); 
				if(inallheaders)
					FixAutoSizeWidth(_oldwidth, _headers, _cols); 
				else
					FixAutoSizeWidth(_oldwidth, null, _cols); 
			}		
			_colswidth = new Array();
			updateColumnsDefinition(_colswidth); 
			AutoSizeItems(_colswidth);
		}
	}
	function AutoSizeColumns(headers)	
	{	
		if(!getIsVisible())
			return; 			
			
		if(headerType == 0 && columnsets != null) 
			columnsets.AutoSizeColumns(headers); 
		else if(headerType == 1)	
			AutoSizeByColumns(headers);	
	}	
	function AutoSizeColumnsAfterDisplay()
	{
		var _header = null; 
		var _headers = getGridEXTable().getHeaders(); 
		if(_headers.length != null)
		{
			var _tmpHeader = null;
			var _headersLength = _headers.length; 			
			for(var _headerIndex = 0; _headerIndex < _headersLength && _header == null; _headerIndex++)
			{
				_tmpHeader = _headers[_headerIndex];
				if(_tmpHeader.getIndex() != getIndex() && _tmpHeader.getIsVisible()) //  && _tmpHeader.getIsAutoSized()
					_header = _tmpHeader; 
			}
		}
		else
		{
			if(_headers.getIndex() != getIndex() && _headers.getVisible() && _headers.getIsAtuoSized())
				_header = _headers; 
		}		
		if(_header != null)		
			AutoSizeColumnsWithHeader(_header);
		else
			AutoSizeColumns(null); 
	}
	function ColumnAutoSize(column, htmlColumn)
	{
		if(column == null || !column.getAllowSize())
			return;		
		if(headerType == 1)
		{
			if(htmlColumn == null)
				htmlColumn = getHtmlColumnById(column.getClientID());		
			
			htmlColumn.style.cursor = "default"; 
			var maxsize = getMaximumColumnSize(column);
			if(maxsize <= 0)
				return; 
			
			if(htmlColumn.getAttribute("type") == "ch")
				maxsize += getGridEXTable().getHeaderWidth(); 
			else if(htmlColumn.getAttribute("pec") != null)
				maxsize += 18; 
			
			AutoSizeColumn(htmlColumn, maxsize);
			resetRootTableScroll(getGridEX().getRootTable()); 
		}
		else
		{
			var cc = getHtmlColumnInColumnSetById(column.getClientID()); 
			htmlColumn = cc[0];
			var i = cc[1];
			var cs = columnsets.getColumnSetInIndex(i);
			cs.AutoSizeColumn(column, htmlColumn); 
		}		
	}
	function CheckSelectors(columnID,checked,updateStatus)
	{		
		var cell = null; 
		var column = null; 
		var columns = getGridEXTable().getColumns(); 
		var columnscount = columns.Count(); 
		for(var col = 0; col < columnscount; col++)
		{
			column = columns.getGridEXColumn(col); 
			if((columnID != null && column.getActAsSelector() && column.getClientID() != columnID) || (columnID == null && column.getActAsSelector()))
			{					
				cell = getHtmlColumnById(column.getClientID()); 				
				if(cell.childNodes.length == 1 && cell.childNodes[0].childNodes.length > 0)
				{									
					var _span = cell.childNodes[0].getElementsByTagName("SPAN")[0];
					var element = _span.childNodes[0]; 					
					if(element.nodeType == 1 && element.tagName == "INPUT" && element.getAttribute("type") == "checkbox")
						element.checked = checked; 
				}
			}
		}		
		getGridEXTable().setSelectorStatus(checked);		
		if(updateStatus)
		{		
			var _data = ""; 
			var _table = null; 
			var l = getGridEX().getTables().Count(); 
			for(var i = 0; i < l; i++)
			{			
				_table = getGridEX().getTables().getTableInIndex(i); 
				if(_table.getSelectorStatus()  || _table == getGridEXTable())
				{
					if(_data.length > 0)
						_data += "|"; 
						
					_data += _table.getID(); 
					_data += ",";
					_data += checked ? "1" : "0"; 
				}
			}			
			var _input = window.document.getElementsByName(getGridEX().getID() + "_selectordata")[0]; 
			if(_input == null)
				throw Error("unable to find selector data"); 				
		
			_input.value = _data;
		}
	}
	function DropColumn(column)
	{
		if(column == null) 
			throw Error("column for drop is null");
		
		if(columndragMode == 3)
		{								
			var cancel = getGridEX().FireEvent("GroupsChanging", [getGridEXTable().getColumns().getGridEXColumnByClientID(currcolumn.getAttribute("id")), 2]);
			if(cancel == null || !cancel)
			{
				var _gridexID = getGridEX().getID(); 
				setGroupEventData(getGridEX().getID(), 2, currgrouptable, currcolumn.getAttribute("name"), "null", "null", columnfordrop.getAttribute("pos"));						
				getGridEX().DoPostBack(null, "GroupsChanging");
			}
			endColumnDrag();
		}
		else
		{				
			if(columnfordrop == null)
				return; 
		
			var direction = 1;			
			var lowpos = parseInt(column.getAttribute("pos"), 10);
			var highpos = parseInt(columnfordrop.getAttribute("pos"), 10); 			
			var _colX = getColumnInPosition(lowpos);
			var _colY = getColumnInPosition(highpos); 			
			var columnX = getGridEXTable().getColumns().getGridEXColumnByClientID(_colX.getAttribute("id")); 
			var columnY = getGridEXTable().getColumns().getGridEXColumnByClientID(_colY.getAttribute("id"));			
			if(columnX.getKeepColumnExpand() || columnY.getKeepColumnExpand())
			{	
				var input = null; 
				if(columnX.getKeepColumnExpand())
				{					
					input = document.getElementById(getGridEX().getID() + "_eventdata");
					input.value = getGridEXTable().getID() + ":" + lowpos + ":" + highpos + ":" + _colY.id;
					getGridEX().DoPostBack(null, "ExpandColumnSwapping"); 				
					window.event.cancelBubble = true; 
					window.event.returnValue = false;
				}
				else if(columnY.getKeepColumnExpand())
				{					
					input = document.getElementById(getGridEX().getID() + "_eventdata"); 
					input.value = getGridEXTable().getID() + ":" + highpos + ":" + lowpos + ":" + _colX.id;															
					getGridEX().DoPostBack(null, "ExpandColumnSwapping"); 				
					window.event.cancelBubble = true; 
					window.event.returnValue = false; 					
				}
				return;
			}						
			if(lowpos > highpos) 
				direction = -1;
			else 
				direction = 1;
				
			var adjustType = -1; 
			var inallheaders = getGridEXTable().getHeaders().length > 1;			
			var _iposoffset = 1; 
			var _cell = null;
			var _cols = htmlTable.getElementsByTagName("COL"); 
			var _diff = 0;
			var _ilowpos = -1;
			var _iposoffset = 1; 
			var _xindex = -1;
			var _yindex = -1; 						
			var _headerColX = null;
			var _headerColY = null; 	
			do
			{
				if(lowpos != highpos)
				{					
					adjustType = -1;
					_colX = getColumnInPosition(lowpos);
					if(_colX == null)
						lowpos += direction;
					else
					{
						_colY = getColumnInPosition(lowpos + direction, false);
						if(_colY == null)					
						{
							_ilowpos = lowpos + direction;
							do
							{
								if(_colY == null && _ilowpos != highpos)
								{
									_ilowpos += direction; 
									_colY = getColumnInPosition(_ilowpos, false); 
									if(_colY == null)
										_iposoffset++;								
								}
							} while(_colY == null && _ilowpos != highpos); 
							
							if(_ilowpos == highpos)
								return; 
						}
						_xindex = _colX.cellIndex;					
						_yindex = _colY.cellIndex;					
						_headerColX = _cols[_colX.cellIndex]; 
						_headerColY = _cols[_colY.cellIndex]; 
						if(_colX.getAttribute("type") != null && _colX.getAttribute("type") == "ch")
						{
							adjustType = 1; 
							swapColumnHeader(_colX, _colY);
						}
						else if(_colY.getAttribute("type") != null && _colY.getAttribute("type") == "ch")
						{
							adjustType = 0; 
							swapColumnHeader(_colY, _colX);															
						}
						_colX.swapNode(_colY); 
						_headerColX.swapNode(_headerColY); 
						if(adjustType == 1)
						{
							_headerColY.width = (getPixelColWidth(_headerColY.width) + getGridEXTable().getHeaderWidth()) + "px"; 
							_headerColX.width = (getPixelColWidth(_headerColX.width) - getGridEXTable().getHeaderWidth()) + "px"; 
						}
						else if(adjustType == 0)
						{
							_headerColX.width = (getPixelColWidth(_headerColX.width) + getGridEXTable().getHeaderWidth()) + "px"; 
							_headerColY.width = (getPixelColWidth(_headerColY.width) - getGridEXTable().getHeaderWidth()) + "px"; 
						}
						columnX = getGridEXTable().getColumns().getGridEXColumnByClientID(_colX.id);
						columnY = getGridEXTable().getColumns().getGridEXColumnByClientID(_colY.id);
						columnY.position = lowpos;
						columnX.position = lowpos + direction;
						_colY.setAttribute("pos", lowpos); 
						_colX.setAttribute("pos", lowpos + direction);
						SwapItems(_colX, _headerColY.getAttribute("id"), _colY, _headerColX.getAttribute("id"));
						if(inallheaders)
							SwapColumnInHeaders(_colX.getAttribute("id"), lowpos + direction, _colY.getAttribute("id"), lowpos); 
										
						lowpos += (direction*_iposoffset); 
					}
				}
			}	while(lowpos != highpos)			
			updateColumnsDefinition(null);
			getGridEXTable().getGridEX().FireEvent("ColumnMoved", [getGridEXTable().getColumns().getGridEXColumnByClientID(column.getAttribute("id"))]);
			if(getGridEXTable().getGridEX().cmpb)
			{
				var input = document.getElementById(getGridEX().getID() + "_eventdata");
				if(input == null)
					throw Error("column moved info field is null");
					
				input.value = getGridEXTable().getID() + ":" + column.getAttribute("id");
				getGridEX().DoPostBack(null, "ColumnMoved"); 
			} 
		}
	}	
	function HitTestColumns(x, y)	
	{					
		if(haveColumnSets)
			return null; 					
	
		var cell = null; 	
		var _gridex = getGridEXTable().getGridEX(); 
		var divtable = getGridEXTable().getHtmlDiv(); 		
		var xlow = -1; 
		var xhigh = -1;
		var ylow = -1;
		var yhigh = -1; 		
		ylow = _gridex.getPixelTop();				
		if(getIsRoot())
		{
			xlow = htmlTable.offsetLeft + divtable.offsetLeft + _gridex.getPixelLeft();
			x += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
			ylow += divtable.offsetTop; 			
		}
		else
		{
			var _htmltable = getGridEXTable().getHtmlItemsTable().parentElement; 
			xlow = _gridex.getPixelLeft() + divtable.offsetLeft;
			if(htmlTable.offsetParent != null)
				xlow += htmlTable.offsetParent.offsetLeft;
			x += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
			ylow += getHierarchicalRowTop(htmlTable) + _htmltable.offsetTop + _htmltable.offsetParent.offsetTop;
			ylow -= _htmltable.scrollTop; 
		}		
		xhigh = xlow + htmlTable.offsetWidth;			
		yhigh = ylow + htmlTable.offsetHeight; 		
		if((x >= xlow && x <= xhigh) && (y >= ylow && y <= yhigh))
		{			
			var xcelllow = null;
			var xcellhigh = null;
			var ycelllow = null; 
			var ycellhigh = null; 			
			var _tablecells = htmlTable.getElementsByTagName("TD"); 
			var l = _tablecells.length; 
			for(var i = 0; i < l; i++)
			{
				cell = _tablecells[i];
				if(cell.getAttribute("type") != "rh")
				{	
					xcelllow = xlow + cell.offsetLeft; 
					xcellhigh = xcelllow + cell.offsetWidth; 
					ycelllow = ylow + cell.offsetTop;
					ycellhigh = ycelllow + cell.offsetHeight; 
					if((x >= xcelllow && x <= xcellhigh) && (y >= ycelllow && y <= ycellhigh))	
						return [cell,  gridEXColumnHeaders]; 
				}
			}
		}				
		return null;
	}	
	function ShowColumnForDrop(x, column)
	{
		var _gridex = getGridEXTable().getGridEX(); 
		var divtable = getGridEXTable().getHtmlDiv();
		var _htmltable = null; 		
		var offsetleft = -1;		
		var xpos = -1;
		var ylow = -1; 
		var yhigh = -1;		
		offsetleft = column.offsetLeft + divtable.offsetLeft + _gridex.getPixelLeft();
		if(!getIsRoot())
		{	
			_htmltable = getGridEXTable().getHtmlItemsTable().parentElement; 
			offsetleft += htmlTable.offsetParent.offsetLeft; 
		}			
		x += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;		
		if(x >= offsetleft && x <= offsetleft + (column.offsetWidth / 2))
		{			
			var _htmltablecells = htmlTable.getElementsByTagName("TD"); 
			if(column.cellIndex - 1 >= 0 && _htmltablecells[column.cellIndex-1].getAttribute("type") != "rh") 	
			{					
				if(columndragMode != 3 && currcolumn.id == _htmltablecells[column.cellIndex-1].id)
					return;
					
				xpos = offsetleft - 1;
				ylow = -1;
				xpos -= getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft; 								
				ylow = getPixelTop(column);
				yhigh = ylow + column.offsetHeight; 				
				drawUpArrow(xpos, ylow);
				drawDownArrow(xpos, yhigh);								
				columnfordrop = column;
			}
			else if((column.cellIndex == 0 && column.getAttribute("type") == "ch" && getIsRoot()) || (column.cellIndex == 0 && column.getAttribute("type") != "rh" && !getIsRoot()))
			{
				if(columndragMode != 3 && column.id == currcolumn.id)
					return; 
					
				xpos = offsetleft - 1;
				ylow = -1;
				xpos -= getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft; 								
				ylow = getPixelTop(column);						
				yhigh = ylow + column.offsetHeight; 				
				drawUpArrow(xpos, ylow);
				drawDownArrow(xpos, yhigh);								
				columnfordrop = column;									
			}			
		}
		else if(x > offsetleft + (column.offsetWidth / 2) && x <= offsetleft + column.offsetWidth)		
		{	
			var _htmltablecells = htmlTable.getElementsByTagName("TD"); 
			if(column.cellIndex + 1 < _htmltablecells.length) 
			{	
				if(columndragMode != 3 && currcolumn.id == _htmltablecells[column.cellIndex+1].id)
					return; 
			
				xpos = offsetleft + column.offsetWidth;
				ylow = -1;
				xpos -= getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft; 		
				ylow = getPixelTop(column);	
				yhigh = ylow + column.offsetHeight;				
				drawUpArrow(xpos, ylow);
				drawDownArrow(xpos, yhigh); 				
				columnfordrop = _htmltablecells[column.cellIndex+1]; 
			}
		}
	}
	function ResizeColumnsExcept(columnIndex, oldwidth, newwidth, cols)
	{
		var igcell = 0; 		
		var cell = null; 
		var cellwidth = 0; 		
		var cellswidth = new Array(); 
		var diff = 0; 
		var l = -1;
		if(htmlTable.cells != null)
			l = htmlTable.cells.length; 
		else
			l = getCellsCore(htmlTable).length;
		for(var i = 0; i < l; i++)
		{
			if(i != columnIndex)
			{
				if(htmlTable.cells != null)
					cell = htmlTable.cells[i]; 
				else
					cell = getCellsCore(htmlTable)[i]; 
				 if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
					cellswidth[cellswidth.length] = cell.offsetWidth; 	
			}
		}
		var inallheaders = (getGridEXTable().getHeaders().length > 1);
		for(var i = 0; i < l; i++)
		{
			if(i != columnIndex)
			{
				if(htmlTable.cells != null)
					cell = htmlTable.cells[i];
				else
					cell = getCellsCore(htmlTable)[i];
				if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
				{
					cellwidth = Math.round((cellswidth[igcell] * newwidth) / oldwidth);	
					diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);				
					cell.childNodes[0].style.width = cellwidth + "px"; 
					getInnerSpan(cell.childNodes[0]).style.width = (cellwidth - diff) + "px";
					cols[i].width =  cellwidth + "px";					
					if(inallheaders)
						ResizeColumnInHeaders(cell, cellwidth - diff, cellwidth);
					
					igcell++; 
				}				
			}
		}
	}
	function ResizeColumnInHeaders(column, cellWidth, colWidth)
	{		
		var _cellWidth;
		var _colWidth; 
		var arrheaders = getGridEXTable().getHeaders();
		var _arrheadersLength = arrheaders.length;
		var _cell = null;
		var _htmlheader = null;		
		var _cols = null; 
		var _sumwidth = 0; 
		for(var iheader = 0; iheader < _arrheadersLength; iheader++)
		{			
			if(headerindex != iheader)
			{
				_htmlheader = arrheaders[iheader].getHtmlHeader();				
				_cols = _htmlheader.getElementsByTagName("COL");
				if(_htmlheader.cells != null)
					_cell = _htmlheader.cells[column.cellIndex]; 
				else
					_cell = getCellsCore(_htmlheader)[column.cellIndex];
				if(column.getAttribute("type") != null && column.getAttribute("type") == "ch" && _cell.getAttribute("type") != "ch")
				{					
					_cellWidth = cellWidth - getGridEXTable().getHeaderWidth(); 
					_colWidth = colWidth - getGridEXTable().getHeaderWidth(); 
					if(_cell.getAttribute("pec") != null && _cell.getAttribute("type") != "ch")
					{
						_cellWidth += 18;
						_colWidth += 18;
					}						
					_cell.childNodes[0].style.width = _colWidth + "px"; 
					getInnerSpan(_cell.childNodes[0]).style.width = _cellWidth + "px"; 					
					_cols[_cell.cellIndex].width = _colWidth + "px";
					_cell.style.width = _colWidth + "px";
				}
				else if(column.getAttribute("type") == null && _cell.getAttribute("type") != null && _cell.getAttribute("type") == "ch")				
				{				
					_cellWidth = cellWidth + getGridEXTable().getHeaderWidth(); 		
					_colWidth = colWidth + getGridEXTable().getHeaderWidth(); 		
					if(_cell.getAttribute("pec") != null && _cell.getAttribute("type") != "ch")
					{
						_cellWidth += 18;
						_colWidth += 18; 
					}						
					_cell.childNodes[0].style.width = _colWidth + "px"; 
					getInnerSpan(_cell.childNodes[0]).style.width = _cellWidth + "px"; 
					_cols[_cell.cellIndex].width = _colWidth + "px"; 
					_cell.style.width = _colWidth + "px";
				}
				else
				{
					_cell.childNodes[0].style.width = colWidth + "px"; 
					getInnerSpan(_cell.childNodes[0]).style.width = cellWidth + "px"; 
					_cols[_cell.cellIndex].width = colWidth + "px"; 
					_cell.style.width = colWidth + "px"; 
				}				
				_sumwidth = 0; 
				for(var _icol = 0; _icol < _cols.length; _icol++)
					_sumwidth += getPixelColWidth(_cols[_icol].width); 
				_htmlheader.style.width = _sumwidth + "px"; 
			}
		}		
	}
	function AutoSizeColumn(column, width)
	{					
		var _width = -1;
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var _colswidth = null; 
		var diff = getPaddingLeft(column) + getPaddingRight(column) + getBorderWidth(column) + getSortWidth(column); 
		var _headers = getGridEXTable().getHeaders(); 
		var _oldcolumnwidth = -1;
		var _oldremainwidth = -1; 
		var _oldwidth = -1; 
		var _newremainwidth = -1; 
		var inallheaders = _headers.length > 1; 
		if(getGridEX().getColumnAutoResize())
		{
			_oldcolumnwidth = column.offsetWidth; 
			_oldwidth = htmlTable.offsetWidth; 
			_oldremainwidth = _oldwidth - _oldcolumnwidth; 			
		}
		_width = width - diff; 
		column.childNodes[0].style.width = width + "px";
		getInnerSpan(column.childNodes[0]).style.width = _width + "px";
		_cols[column.cellIndex].width = width + "px"; 
		column.style.width = width + "px"; 
		if(inallheaders)
			ResizeColumnInHeaders(column, _width, width); 
			
		if(getGridEX().getColumnAutoResize())
		{
			_newremainwidth = _oldwidth - width; 
			ResizeColumnsExcept(column.cellIndex, _oldremainwidth , _newremainwidth, _cols); 
			if(inallheaders)
				FixAutoSizeWidth(_oldwidth, _headers, _cols, false); 
			else
				FixAutoSizeWidth(_oldwidth, null, _cols, false); 
		}		
		_colswidth = new Array();
		updateColumnsDefinition(_colswidth); 
		AutoSizeItems(_colswidth, false);
	}
	function ResizeColumnWidth(column, posX)	
	{						
		var _gridex = getGridEXTable().getGridEX(); 
		posX += getScrollLeft(_gridex); 
		var cellindex = column.cellIndex;						
		var offsetwidth = posX - (column.offsetWidth + getPixelLeft(column));				
		if(!getIsRoot())
			offsetwidth += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;			

		var _colswidth	 = null; 
		var _cols = null; 
		var _headers = getGridEXTable().getHeaders();
		var _width = null; 		
		var diff = null;
		var oldremainwidth = null; 
		var newremainwidth = null; 
		var newwidth = null; 		
		var inallheaders = _headers.length > 1; 
		var oldwidth = htmlTable.offsetWidth; 
		var _leftmin = getMinLeft(column, getGridEXTable()); 		
		if(_gridex.isDropDown())
		{
			if(posX > getPixelLeft(htmlTable) + oldwidth)
				return; 
		}
		_cols = htmlTable.getElementsByTagName("COL"); 	
		var l = -1;
		if(htmlTable.cells != null)
			l = htmlTable.cells.length;
		else
			l = getCellsCore(htmlTable).length; 
		if(cellindex + 1 < l)		
		{	
			if(getGridEX().getColumnAutoResize())
			{
				oldremainwidth = GetCellsWidth(cellindex + 1, l - 1);
				if((column.offsetWidth + offsetwidth) < _leftmin)
					newremainwidth = oldremainwidth - (_leftmin - column.offsetWidth); 
				else
					newremainwidth = oldremainwidth - offsetwidth;		
			}			
			diff = getPaddingLeft(column) + getPaddingRight(column) + getBorderWidth(column) + getSortWidth(column); 
			newwidth = column.offsetWidth + offsetwidth;
			if(newwidth < _leftmin)
				newwidth = _leftmin; 			
			_width = newwidth - diff;		
			column.childNodes[0].style.width = (newwidth) + "px"; 	
			getInnerSpan(column.childNodes[0]).style.width = (_width) + "px"; 
			_cols[column.cellIndex].width = newwidth + "px"; 
			column.style.width = (newwidth) + "px";									
			if(inallheaders)
				ResizeColumnInHeaders(column, _width, newwidth); 

			if(getGridEX().getColumnAutoResize())
			{										
				ResizeColumns(cellindex + 1, l - 1, oldremainwidth, newremainwidth, _cols);						 	
				if(inallheaders)
					FixAutoSizeWidth(oldwidth, _headers, _cols, false); 
				else
					FixAutoSizeWidth(oldwidth, null, _cols, false); 
			}			
			_colswidth = new Array(); 
			updateColumnsDefinition(_colswidth);	
			AutoSizeItems(_colswidth);						
		}
		else
		{
			if(getGridEX().getColumnAutoResize())
			{
				oldremainwidth = GetCellsWidth(0, cellindex - 1); 			
				newremainwidth = oldremainwidth - offsetwidth;			
			}
			diff = getPaddingLeft(column) + getPaddingRight(column) + getBorderWidth(column) + getSortWidth(column); 
			newwidth = column.offsetWidth + offsetwidth;
			if(newwidth < _leftmin)
				newwidth = _leftmin; 
			_width = newwidth - diff;			
			column.childNodes[0].style.width = newwidth + "px";
			getInnerSpan(column.childNodes[0]).style.width = _width + "px"; 
			_cols[column.cellIndex].width = newwidth + "px"; 
			column.style.width = newwidth + "px"; 
			if(inallheaders)
				ResizeColumnInHeaders(column, _width, newwidth); 
			
			if(getGridEX().getColumnAutoResize())
			{
				ResizeColumns(0, cellindex - 1, oldremainwidth, newremainwidth, _cols); 			
				if(inallheaders)
					FixAutoSizeWidth(oldwidth, getGridEXTable().getHeaders(), _cols, false); 
				else
					FixAutoSizeWidth(oldwidth, null, _cols, false);
			}			
			_colswidth = new Array(); 	
			updateColumnsDefinition(_colswidth);				
			AutoSizeItems(_colswidth);
		}		
		var _w = 0; 
		for(var i=0;i<_cols.length;i++)			
			_w += getPixelWidth(_cols[i].width); 		
		if(_w > 0)		
			htmlTable.style.width = _w + "px"; 		
		resetRootTableScroll(getGridEX().getRootTable()); 		
	}
	function getFixedHierarchyWidth()
	{
		var width = 0; 
		var headerIndex = htmlTable.parentElement.parentElement.cellIndex; 
		var t = htmlTable.parentElement.parentElement.offsetParent; 
		var l = -1;
		if(t.cells != null)
			l = t.cells.length;
		else
			l = getCellsCore(t).length; 
		for(var i=0; i<l; i++)
		{
			if(i != headerIndex)
			{
				if(t.cells != null)
					width += t.cells[i].offsetWidth; 
				else
					width += getCellsCore(t)[i].offsetWidth;
			}
		}		
		return width; 
	}			
	function updateColumnsDefinition(columnsWidth)
	{				
		var _field = document.getElementsByName(getGridEXTable().getID() + "_cols")[0];
		if(_field == null)
			throw Error("input field for columns definition is null");
			
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var _length = -1;
		if(htmlTable.cells != null)
			 _length = htmlTable.cells.length; 		
		else
			_length = getCellsCore(htmlTable).length;
		var _width = null; 
		var cell = null;
		var _colindex = 0;
		for(var i = 0; i < _length; i++)
		{			
			if(htmlTable.cells != null)
				cell = htmlTable.cells[i];
			else
				cell = getCellsCore(htmlTable)[i]; 
			if(cell.getAttribute("id") != null && cell.getAttribute("pos") != null)
			{				
				if(cell.getAttribute("allowsize") == null && columnsWidth != null)
				{
					_width = cell.offsetWidth; 				
					if(cell.getAttribute("type") == "ch")
						_width -= getGridEXTable().getHeaderWidth();
						
					updateColumnDefinitionInField(_field, cell.getAttribute("id"), parseInt(cell.getAttribute("pos"), 10), _width);
				}
				else if(columnsWidth == null)
					updateColumnDefinitionInField(_field, cell.getAttribute("id"), parseInt(cell.getAttribute("pos"), 10), -1);	
			}				
			if(columnsWidth != null)
			{
				if(cell.getAttribute("type") != "rh")				
				{				
					_width = getPixelWidth(_cols[i].width);
					_t += _width;
					if(cell.getAttribute("type") == "ch")
						_width -= getGridEXTable().getHeaderWidth();						
					
					columnsWidth[columnsWidth.length] = _cols[i].id;
					columnsWidth[columnsWidth.length] = _width; 										
				}
			}
		}
	}			
	function AutoSizeItems(columnsWidth, resizeTable)
	{	
		var _itemsTables = null;
		if(document.getChildsById != null)
			_itemsTables = document.getChildsById(getGridEXTable().getID() + "_i"); 
		else
			_itemsTables = allChildsCore(getGridEXTable().getID()+"_i",document.getElementsByTagName("*")); 
		var _itemsCols = null; 
		var _itemsColsLength = -1; 
		var _colwidthLength = columnsWidth.length; 		
		var _customApplied = false;
		var _itemCol = null; 
		var _itemTable = null; 
		var _itemsTablesLength = _itemsTables.length; 
		var _width = -1; 
		var _tablewidth = 0; 
		if(_itemsTablesLength > 0)
		{			
			for(var _item = 0; _item < _itemsTablesLength; _item++)
			{											
				_itemTable = _itemsTables[_item];
				_itemsCols = _itemTable.getElementsByTagName("COL"); 				
				for(var _icol = 0;  _icol < _colwidthLength; _icol = _icol + 2)
				{					
					_itemCol = getItem(_itemsCols, columnsWidth[_icol]); 										
					if(_itemCol.getAttribute("type") != "space")
					{
						if(!_customApplied && _itemCol.getAttribute("iscz")  != null)
						{
							_width = (columnsWidth[_icol+1] + getGridEXTable().getHeaderWidth());
							if(getPixelColWidth(_itemCol.width) != _width)
							{
								_itemCol.width = _width + "px"; 
								_customApplied = true; 
							}
						}
						else											
						{							
							_width = columnsWidth[_icol+1];														
							if(getPixelColWidth(_itemCol.width) != _width)							
								_itemCol.width = _width + "px";	
						}
					}
				}				
				if(resizeTable == null || resizeTable == true)
				{
					_tablewidth = 0; 		
					_itemsColsLength = _itemsCols.length;					
					for(var _icol = 0; _icol < _itemsColsLength; _icol++)						
						_tablewidth += getPixelColWidth(_itemsCols[_icol].width);
					_itemTable.style.width = (_tablewidth) + "px";
					if(_itemTable.parentElement.tagName == "TD")
					{	
						var e = null; 
						var _pwidth = 0; 
						var _pcols = null;
						if(_itemTable.offsetParent == null)
						{
							e = _itemTable.parentElement;
							while(e != null && e.tagName != "TABLE")
								e = e.parentElement;
								
							if(e != null)
								_pcols = e.getElementsByTagName("COL"); 
							else
								return; 
						}
						else
							_pcols = _itemTable.offsetParent.getElementsByTagName("COL");
						for(var _i = 0; _i < _pcols.length - _itemsColsLength; _i++)
						{
							if(_i == _itemTable.parentElement.cellIndex)
								_pcols[_i].width = _tablewidth + "px";
							_pwidth += getPixelColWidth(_pcols[_i].width); 							
						}
						if(_pwidth > 0)
						{
							if(_itemTable.offsetParent != null)
								_itemTable.offsetParent.style.width = _pwidth + "px";
							else
								e.style.width = _pwidth + "px"; 
						}
					}					
				}
				else
				{					
					var _oldwidth = getPixelWidth(_itemTable.style.width);
					_itemTable.style.width = _oldwidth - 1;
					_itemTable.style.width = _oldwidth; 
				}
			}			
		}
		if(getGridEXTable().getParent() == null)
		{
			var _htmlitemstable = getGridEXTable().getHtmlItemsTable(); 
			if(_htmlitemstable.getAttribute("empty") != null)
				_htmlitemstable.style.width = getGridEXTable().getWidth() + "px"; 
		}
		var _newdiv = null; 
		if(document.getChildsById != null)
			_newdiv = document.getChildsById("nrsep" + getGridEXTable().getID()); 
		else
			_newdiv = allChildsCore("nrsep" + getGridEXTable().getID(), document.getElementsByTagName("*")); 
		if(_newdiv != null)
		{
			var l = _newdiv.length; 
			for(var i = 0; i < l; i++)
				_newdiv[i].style.width = getGridEXTable().getWidth() + "px"; 
		}
		var thdiv = null;
		if(document.getChildsById != null)
			thdiv = document.getChildsById("th" + getGridEXTable().getID());
		else	
			thdiv = allChildsCore("th" + getGridEXTable().getID(), document.getElementsByTagName("*")); 
		if(thdiv != null)
		{
			var l = thdiv.length; 
			for(var i=0;i<l;i++)
			{
				var thwidth = getGridEXTable().getWidth();
				if(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset") != null)
					thwidth -= parseInt(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset"), 10);	
				thdiv[i].getElementsByTagName("COL")[0].width = thwidth + "px";
				thdiv[i].style.width = thwidth + "px"; 				
			}
		}		
		if(getGridEX().getResizeGroups())
		{
			var _offset = 0; 
			var _tablewidth = getGridEXTable().getWidth();
			if(document.getChildsById != null)
				_itemsTables = document.getChildsById("group"+getGridEXTable().getID()); 
			else
				_itemsTables = allChildsCore("group"+getGridEXTable().getID(),document.getElementsByTagName("*")); 
			_itemsTablesLength = _itemsTables.length; 
			if(_itemsTablesLength > 0)
			{
				for(var i = 0; i < _itemsTablesLength; i++)
				{										
					_itemsCols = _itemsTables[i].getElementsByTagName("COL"); 
					_itemCol = _itemsCols[0]; 
					_offset = _itemCol.getAttribute("offset"); 
					_itemCol.width = (_tablewidth - _offset) + "px"; 
					_itemsTables[i].style.width = (_tablewidth - _offset) + "px"; 
				}
			}
		}
		if(getGridEX().getFixTableSize())
			fixTableSize(getGridEX().getRootTable()); 		
	}
	function AutoSizeByColumns(headers)	
	{			
		var width = getGridEX().getResizeWidth(); 
		if(width <= 0)
			return; 
		
		if(!getIsRoot())
			width -= getFixedHierarchyWidth(); 
		
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var _childcols = null; 
		var _childwidth = -1; 
		var fixedwidth = getFixedWidth();
		var oldwidth = htmlTable.offsetWidth - fixedwidth; 
		var newwidth = width - fixedwidth;		
		var cell = null; 
		var _cell = null;		
		var length = -1;
		if(htmlTable.cells != null)
			length = htmlTable.cells.length; 
		else
			length = getCellsCore(htmlTable).length;
		var cellswidth = new Array();
		for(var icell = 0; icell < length; icell++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)		
				cellswidth[cellswidth.length] = getPixelColWidth(_cols[icell].width); 
		}
		var igcell = 0;
		var diff = 0;
		var cellsize = 0;		
		var _headersLength = -1; 
		var _table = null; 		
		for(var icell = 0; icell < length; icell++)
		{	
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null) 
			{	
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
				cellsize = Math.round((cellswidth[igcell] * newwidth) / oldwidth);	
				if(cellsize - diff > 0 || (diff - getSortWidth(cell) > 0))
				{	
					cell.childNodes[0].style.width = cellsize + "px";
					getInnerSpan(cell.childNodes[0]).style.width = (cellsize - diff) + "px"; 
					cell.style.width = cellsize + "px"; 
					_cols[icell].width = cellsize + "px";
					if(headers != null && headers.length > 1)
					{
						if(cellsize - diff > 0)
						{
							cell.childNodes[0].style.pixelWidth = cellsize - diff;																				
							_cols[icell].width = cellsize  + "px"; 				
						}
						else
						{
							var _fixedwidth = 0; 
							if(cellsize - diff ==  0) 
								_fixedwidth = diff - getSortWidth(cell);
							else
							{							
								if(cellsize <= 0)
									cellsize = diff - getSortWidth(cell);
								_fixedwidth = cellsize; 
							}
							cell.childNodes[0].style.pixelWidth = _fixedwidth;
							_cols[icell].width =  _fixedwidth + "px";
						}					
						_headersLength = headers.length; 
						_table = null; 					
						for(var iheader = 1; iheader < _headersLength; iheader++)
						{
							if(headers[iheader].getIsVisible())
							{
								_table = headers[iheader].getHtmlHeader();
								_childcols = _table.getElementsByTagName("COL");
								if(_table.cells != null)
									_cell = _table.cells[icell]; 							
								else
									_cell = getCellsCore(_table)[icell];
								if(cell.getAttribute("type") == "ch" && _cell.getAttribute("type") != "ch")								
								{		
									var _usewidth = 0; 
									if(cellsize - diff > 0)
										_usewidth = cellsize - diff;
									else if(cellsize - diff == 0 || cellsize <= 0)
										_usewidth = diff - getSortWidth(cell);
									else
										_usewidth = cellsize; 
												
									_childwidth = _usewidth - getGridEXTable().getHeaderWidth(); 		
									if(_cell.getAttribute("pec") != null && _cell.getAttribute("type") != "ch")
										_childwidth += 18;
										
									_cell.childNodes[0].style.width = (_childwidth + diff) + "px"; 
									getInnerSpan(_cell.childNodes[0]).style.width = _childwidth + "px"; 
									_cell.style.width = (_childwidth + diff) + "px";
									_childcols[_cell.cellIndex].width = (_childwidth + diff) + "px";
								}
								else 
								{					
									if(cellsize - diff > 0)
									{			
										_cell.childNodes[0].style.width = cellsize + "px";
										getInnerSpan(_cell.childNodes[0]).style.width =  (cellsize - diff) + "px";
										_cell.style.width = cellsize + "px";
										_childcols[_cell.cellIndex].width = cellsize + "px"; 
									}
									else
									{
										var _usewidth = 0;
										 if(cellsize - diff == 0 || cellsize <= 0)
											_usewidth = diff - getSortWidth(cell);
										else
											_usewidth = cellsize; 
											
										_cell.childNodes[0].style.width = _usewidth + "px";
										getInnerSpan(_cell.childNodes[0]).style.width =  _usewidth + "px";
										_cell.style.width = _usewidth + "px";
										_childcols[_cell.cellIndex].width = _usewidth + "px"; 										
									}
								}
							}
						}
					}
				}
				igcell++; 
			}
		}
		updateTableSize(htmlTable, _cols);
		if(headers != null && headers.length > 1)
		{
			for(var iheader = 1; iheader < _headersLength; iheader++)
			{
				if(headers[iheader].getIsVisible())
					updateTableSize(headers[iheader].getHtmlHeader(), null); 
			}			
		} 
		FixAutoSizeWidth(width, headers, _cols);									
		var columnsWidth = new Array(); 
		updateColumnsDefinition(columnsWidth); 
		AutoSizeItems(columnsWidth);
	}			
	function FixAutoSizeWidth(newwidth, headers, cols, resizeHeader)
	{		
		var row = htmlTable.rows[0]; 
		var width = htmlTable.offsetWidth;	
		var colslength = cols.length; 
		var _col = null;
		var _childcols = null; 
		var _childcol = null; 	
		var length = row.cells.length;		
		var countzero = 0;		
		var diffsize = newwidth - width; 
		var offset = -1; 		
		if(diffsize < 0) 
			offset = -1;
		else if(diffsize > 0) 
			offset = 1; 		
		else
			return; 
		
		var cell = null; 
		var _childcell = null; 	
		var _childwidth = -1; 
		var _length = (headers != null) ? headers.length : -1;		
		var _lowwidth = 0;
		var _style = null; 
		var _table = null;
		var _width = 0;
		do
		{
			for(var index = 0; index < length && (diffsize != 0 && countzero != cellsresize); index++)
			{		
				cell = row.cells[index];
				if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
				{						
					_childcell = getInnerSpan(cell.childNodes[0]);					
					if(cell.getAttribute("type") == "ch")
						_lowwidth = getGridEXTable().getHeaderWidth(); 												
					else
					{						
						if(cell.getAttribute("pec") != null)
							_lowwidth = 18; 
						else
							_lowwidth = 0;
					}
					_style = _childcell.style;	
					if(getPixelWidth(_style.width) == 0)
					{
						if(isDefaultView)
						{
							if(document.defaultView.getComputedStyle(_childcell, null).getPropertyValue("width") != null)
								_width = getPixelWidth(document.defaultView.getComputedStyle(_childcell, null).getPropertyValue("width")); 						
						}
						else
							_width = getPixelWidth(_childcell.style.width); 
					}
					else
						_width = getPixelWidth(_style.width); 
						
					if(_width + offset > _lowwidth)
					{						
						_style.width = (_width + offset) + "px";
						_col = cols[index]; 
						cell.childNodes[0].style.width = (getPixelColWidth(_col.width) + offset) + "px";
						cell.style.width = (getPixelColWidth(_col.width) + offset) + "px"; 						
						_col.width = (getPixelColWidth(_col.width) + offset) + "px";
						if(headers != null)
						{														
							for(var iheader = 0; iheader < _length; iheader++)
							{
								if(iheader != headerindex)
								{
									_table = headers[iheader].getHtmlHeader();
									_childcols = _table.getElementsByTagName("COL");
									if(_table.cells != null)
										_childcell = getInnerSpan(_table.cells[index].childNodes[0]);
									else
										_childcell = getInnerSpan(getCellsCore(_table)[index].childNodes[0]);
									if(getPixelWidth(_childcell.style.width) == 0)
									{										
										if(isDefaultView)
										{
											if(document.defaultView.getComputedStyle(_childcell, null).getPropertyValue("width") != null)
												_childwidth = getPixelWidth(document.defaultView.getComputedStyle(_childcell,null).getPropertyValue("width"));
										}
										else
											_childwidth = getPixelWidth(_childcell.style.width); 
									}
									else
										_childwidth = getPixelWidth(_childcell.style.width);
										
									_childwidth += offset; 
									_childcell.style.width = _childwidth + "px";
									_childcol = _childcols[index]; 
									if(_table.cells != null)
									{
										_table.cells[index].childNodes[0].style.width = (getPixelColWidth(_childcol.width) + offset) + "px"; 
										_table.cells[index].style.width = (getPixelColWidth(_childcol.width) + offset) + "px";
									}
									else
									{
										getCellsCore(_table)[index].childNodes[0].style.width = (getPixelColWidth(_childcol.width) + offset) + "px"; 
										getCellsCore(_table)[index].style.width = (getPixelColWidth(_childcol.width) + offset) + "px";
									}
									_childcol.width = (getPixelColWidth(_childcol.width) + offset) + "px"; 
								}
							}
						}
						width += offset; 
						diffsize = newwidth - width; 
					}
					else
						countzero++;
				}
			}
		} while(diffsize != 0 && countzero != cellsresize)		
		if(resizeHeader)
		{
			updateTableSize(htmlTable, cols);
			if(headers != null && headers.length > 1)
			{
				for(var i = 1; i < _headersLength; i++)
				{
					if(headers[i].getIsVisible())
						updateTableSize(headers[i].getHtmlHeader(), null); 
				}			
			}
		}		
	}	
	function getColumnInPosition(position, throwError)
	{
		var _tablecells = htmlTable.getElementsByTagName("TD"); 
		var _length = _tablecells.length; 
		var cell = null; 		
		for(var i = 0; i < _length; i++)
		{
			cell = _tablecells[i];
			if(cell.getAttribute("pos") != null)
			{	
				if(parseInt(cell.getAttribute("pos"), 10) == position)
					return cell; 
			}
		}		
		if(throwError == null || throwError)
			throw Error("argument out of range"); 
		else
			return null; 		
	}	
	function GetCellsWidth(lowcell, highcell)
	{
		var width = 0; 
		var cell = null;
		var _tablecells = htmlTable.getElementsByTagName("TD");  
		while(lowcell <= highcell)
		{
			cell = _tablecells[lowcell];
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
				width += cell.offsetWidth; 
				
			lowcell++;
		}
		return width; 
	}	
	function ResizeColumns(lowcell, highcell, oldwidth, newwidth, cols)
	{			
		var cell = null; 
		var cellswidth = new Array(); 
		for(var i = lowcell; i <= highcell; i++)
		{			
			if(htmlTable.cells != null)
				cell = htmlTable.cells[i];
			else
				cell = getCellsCore(htmlTable)[i]; 
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null) 
				cellswidth[cellswidth.length] = cell.offsetWidth;
		}	
		var inallheaders = (getGridEXTable().getHeaders().length > 1);
		var igcell = 0; 
		var cellwidth = 0;
		var diff = 0; 
		while(lowcell <= highcell)
		{			
			if(htmlTable.cells != null)
				cell = htmlTable.cells[lowcell];
			else
				cell = getCellsCore(htmlTable)[lowcell];
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null) 
			{
				cellwidth = Math.round((cellswidth[igcell] * newwidth) / oldwidth);	
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell);				
				cell.childNodes[0].style.width = (cellwidth) + "px";
				getInnerSpan(cell.childNodes[0]).style.width = (cellwidth-diff) + "px";				
				cell.style.width = cellwidth + "px"; 	
				cols[lowcell].width =  cellwidth + "px"; 			
				igcell++; 				
				if(inallheaders)
					ResizeColumnInHeaders(cell, cellwidth - diff);
			}
			lowcell++; 
		}
	}	
	function SetColumnPositionInHeaders(colId, colPos)
	{
		var arrheaders = getGridEXTable().getHeaders(); 		
		var _length = arrheaders.length; 		
		for(var i = 0; i < _length; i++)
		{
			if(headerindex != i)
				arrheaders[i].getHtmlHeader().all(colId).getAttribute("pos") = colPos;
		}
	}
	function swapItemHeaders(colX, colY)
	{		
		if(colX.getAttribute("pec") != null)
			colY.setAttribute("pec", colX.getAttribute("pec")); 
		
		colX.removeAttribute("pec"); 
	}
	function SwapItems(columnX, colXID, columnY, colYID)
	{		
		var _colx = null; 
		var _coly = null; 
		var _cols = null; 				
		var _cellsLength = -1; 
		var _itemtable = null; 
		var _itemstables = null;
		if(document.getChildsById != null)
			_itemstables = document.getChildsById(getGridEXTable().getID() + "_i"); 
		else
			_itemstables = allChildsCore(getGridEXTable().getID() + "_i", document.getElementsByTagName("*"));
		var _itemstablesLength = _itemstables.length; 
		var cellsX = null; 
		var cellsY = null; 
		var xID = columnX.getAttribute("id");
		var yID = columnY.getAttribute("id");
		if(_itemstablesLength > 0)
		{		
			for(var _i = 0; _i < _itemstablesLength; _i++)
			{
				_itemtable = _itemstables[_i];
				if(_itemtable.getAttribute("emtpy") != null)
					return; 
					
				_cols = _itemtable.getElementsByTagName("COL"); 								
				_coly = getItem(_cols,colYID); 
				_colx = getItem(_cols,colXID);				
				if(_coly.getAttribute("pec") != null) 
					swapItemHeaders(_coly, _colx); 
				else if(_colx.getAttribute("pec") != null)
					swapItemHeaders(_colx, _coly);
				
				_coly.swapNode(_colx);
			}
		}
		if(document.getChildsById != null)
		{
			cellsX = document.getChildsById(xID + "_L"); 
			cellsY = document.getChildsById(yID + "_L");
		}
		else
		{
			cellsX = allChildsCore(xID + "_L", document.getElementsByTagName("*"));
			cellsY = allChildsCore(yID + "_L", document.getElementsByTagName("*"));
		}
		_cellsLength = cellsX.length; 
		for(var icell = 0; icell < _cellsLength; icell++)
			cellsY[icell].swapNode(cellsX[icell]); 		
	}	
	function SwapColumnInHeaders(xID, xPos,  yID, yPos)
	{
		var arrheaders = getGridEXTable().getHeaders(); 		
		var colX = null;
		var colY = null;		
		var _cell = null; 
		var _diff = null;
		var _htmlheader = null; 
		var _length = arrheaders.length;
		for(var iheader = 0; iheader < _length; iheader++)
		{
			if(headerindex != iheader)
			{
				_htmlheader = arrheaders[iheader].getHtmlHeader(); 
				colX = _htmlheader.all(xID);
				colY = _htmlheader.all(yID);				
				if(colY.getAttribute("type") != null && colY.getAttribute("type") == "ch")
					swapColumnHeader(colY, colX);
				else if(colX.getAttribute("type") != null && colX.getAttribute("type") == "ch")
					swapColumnHeader(colX, colY);				
				
				colX.swapNode(colY);
				colX.setAttribute("pos",xPos);
				colY.setAttribute("pos",yPos); 
			}
		}
	}		
	function getFixedWidth()
	{
		var l = -1;
		if(htmlTable.cells != null)
			l = htmlTable.cells.length; 
		else
			l = getCellsCore(htmlTable).length; 
		var fixedwidth = 0;
		var cell = null; 
		for(var i = 0; i < l; i++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[i];
			else
				cell = getCellsCore(htmlTable)[i];
			if(cell.getAttribute("type") == "rh" || cell.getAttribute("allowsize") != null)
				fixedwidth += cell.offsetWidth; 
		}
		return fixedwidth; 
	}	
	function column_oncontextmenu()
	{		
		window.event.cancelBubble = true;
		window.event.returnValue = false; 
		return false; 
	}
	function column_onmouseover()
	{		
		var column = getColumnFromElement(window.event.srcElement);
		if(column.getAttribute("allowsize") == null)
		{
			if(columnResizing)
				column.style.cursor = cursorResize;
			else
			{
				if(isInResizeArea(column, htmlTable) && !columnDraging)				
					column.style.cursor = cursorResize;
				else
					column.style.cursor = "default";
			}
		}		
		if(gridEXTable.getGridEX().getThemedAreas() == 9 || gridEXTable.getGridEX().getThemedAreas() == 4)
		{					
			if(!columnResizing && !columnDraging)
			{	
				var _top = getPixelTop(column) + getBorderStyleWidth(column.style.borderTopWidth); 
				var _left = getPixelLeft(column) + getBorderStyleWidth(column.style.borderLeftWidth); 
				var _right = _left + column.clientWidth - getBorderStyleWidth(column.style.borderRightWidth); 
				var _bottom = _top + column.clientHeight - getBorderStyleWidth(column.style.borderBottomWidth); 
				showthemed_columnover(_top, _left, _right, _bottom, column.clientWidth, column.clientHeight);
			}
		} 
	}	
	function column_onmousedown()
	{							
		var column = getColumnFromElement(window.event.srcElement);			
		if(column.getAttribute("id") == null || column.getAttribute("id").length == 0)
			return; 
		
		if(!columnResizing)
		{	
			var gridEXColumn = getGridEXTable().getColumns().getGridEXColumnByClientID(column.id);
			if(gridEXColumn.getAllowSize() || gridEXColumn.getAllowDrag()) 
			{
				if(column.style.cursor == cursorResize && gridEXColumn.getAllowSize())
				{
					couldStartResize = true; 
					couldResizeColumn = column;
					couldResizeHeader = gridEXColumnHeaders;
					resizepoint = new Point(window.event.clientX, window.event.clientY); 
				}
				else if(gridEXColumn.getAllowDrag())
				{	
					if(gridEXColumn.getActAsSelector())
						return; 
						
					if(gridEXTable.getGridEX().getThemedAreas() == 1)
						ShowColumnPressed(column, true);
		
					if(gridEXColumn.getAllowDrag())					
					{
						couldStartDrag = true;
						couldDragColumn = column; 
						couldDragHeader = gridEXColumnHeaders;
						dragpoint = new Point(event.clientX, event.clientY); 
					}						
				}
			}
		}			
		window.event.cancelBubble = true; 
	}	
	function column_onmousemove()
	{	
		if(couldStartDrag && dragpoint != null)
		{		
			if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
			{
				var column = getColumnFromElement(window.event.srcElement); 
				startColumnDrag(column, gridEXColumnHeaders, window.event);
				couldStartDrag = false;
				couldDragColumn = couldDragHeader = null; 				
				dragpoint = null; 
			}
		}
		else if(couldStartResize && resizepoint != null)
		{
			if(Math.abs(window.event.clientX - resizepoint.X()) > 1 || Math.abs(window.event.clientY - resizepoint.Y()) > 1)
			{
				startColumnResize(couldResizeColumn, couldResizeHeader); 
				couldStartResize = false; 
				couldResizeColumn = couldResizeHeader = null;
				resizepoint = null; 
			}
		}
		else	if(!columnResizing)
		{	
			var column = getColumnFromElement(window.event.srcElement); 
			if(column.getAttribute("allowsize") == null)
			{
				if(isInResizeArea(column, htmlTable) && !columnDraging)
					column.style.cursor = cursorResize;
				else
					column.style.cursor = "default"; 
			}
		}
	}
	var eventButton = 0; 
	function column_onmouseup()
	{
		eventButton = window.event.button; 
	}
	function column_onclick()		
	{		
		var tdColumn = getColumnFromElement(window.event.srcElement); 
		if(!columnResizing && !columnDraging && !canceledByUser && tdColumn.style.cursor != cursorResize)
		{					
			var column = getGridEXTable().getColumns().getGridEXColumnByClientID(tdColumn.id);
			if(column.getActAsSelector())
				return;
				
			if(column.getAllowSort())
			{	
				var cancel = getGridEX().FireEvent("ColumnHeaderClick", [column]);
				if(cancel == null || !cancel)
				{
					var input = document.getElementsByName(getGridEX().getID() + "_eventdata")[0]; 			
					if(input == null)
						throw Error("sort info field is null"); 				
				
					input.value = getGridEXTable().getID() + ":" + column.getClientID(); 
					getGridEX().DoPostBack(null, "ColumnHeaderClick");
				}
			}
		}
		else if(columnResizing)
			endColumnResize();			
		else if(columnDraging)
			drag_onmouseup();
			
		if(gridEXTable.getGridEX().getThemedAreas() == 1)
		{			
			ShowColumnUnPressed();
			currpressedcolumn = null;
			couldStartDrag = false;
			couldDragColumn = couldDragHeader = null;
			dragpoint = null; 
		}		
		if(tdColumn.style.cursor != cursorResize)
		{
			window.event.cancelBubble = true;
			window.event.returnValue = false; 
		}
		getGridEXTable().getGridEX().setHitTestArea(5); 
		getGridEXTable().getGridEX().FireEvent("Click", [getGridEXTable().getGridEX(),(window.event.type == "contextmenu") ? 2 : 1, window.event.clientX, window.event.clientY]);
	}	
	function column_onmouseout()
	{		
		var column = getColumnFromElement(window.event.srcElement); 		
		if(gridEXTable.getGridEX().getThemedAreas() == 9 || gridEXTable.getGridEX().getThemedAreas() == 4)
			hidethemed_columnover(); 	
	}	
	function column_ondblclick()
	{		
		cancelColumnResize(); 
		var column = getColumnFromElement(window.event.srcElement); 		
		if(column.style.cursor == cursorResize)
		{
			column.style.cursor = "default"; 
			var gridEXColumn = getGridEXTable().getColumns().getGridEXColumnByClientID(column.getAttribute("id")); 
			var maxColumnSize = getMaximumColumnSize(gridEXColumn); 
			if(maxColumnSize <= 0)
				return; 			
			
			if(column.getAttribute("type") == "ch")
				maxColumnSize += getGridEXTable().getHeaderWidth();
			else if(column.getAttribute("pec") != null)
				maxColumnSize += 18;
				
			AutoSizeColumn(column, maxColumnSize);
		}
		getGridEXTable().getGridEX().setHitTestArea(6);
		getGridEXTable().getGridEX().FireEvent("DoubleClick", [getGridEXTable().getGridEX(), eventButton, window.event.clientX, window.event.clientY]); 
	}
	function header_onselectstart()
	{ }	
	function header_onmousedown()
	{ }	
	if(headerType == 1) 
	{		
		var row = htmlTable.rows[0];
		var cell = null; 
		for(var icell = 0; icell < row.cells.length; icell++)
		{
			cell = row.cells[icell];
			if(cell.getAttribute("type") != "rh")
			{
				cell.addEventListener("mousedown", column_onmousedown, false);
				cell.addEventListener("mousemove", column_onmousemove, false); 
				cell.addEventListener("mouseout", column_onmouseout, false); 
				cell.addEventListener("mouseover", column_onmouseover, false);
				cell.addEventListener("mouseup", column_onmouseup, false);
				cell.addEventListener("click", column_onclick, false); 
				cell.addEventListener("dblclick", column_ondblclick, false); 
				cell.addEventListener("contextmenu", column_oncontextmenu, false); 
			}			
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
				cellsresize++;
		}
	}	
	if(htmlTable != null)
	{
		htmlTable.addEventListener("select", header_onselectstart, false); 
		htmlTable.addEventListener("mousedown", header_onmousedown, false); 
	}	
	var gridEXColumnHeaders = this; 
	return this; 
}
function GridEXChildTableCollection(gridEXTable)
{
	var childTables = new Array(); 
	var gridEXTable = gridEXTable; 
	this.Count = Count; 	
	this.getTableByID = getTableByID; 
	this.getTableInIndex = getTableInIndex;	
	this.Add = Add; 	
	function Count()
	{
		return childTables.length;
	}	
	function Add(childTable)
	{		
		if(getTableByID(childTable.getID()) == null)		
			childTables[childTables.length] = childTable;
	}	
	function getTableByID(id)
	{
		var childTable = null; 
		var _length = childTables.length; 
		for(var index = 0; index < _length; index++)
		{
			childTable = childTables[index]; 
			if(childTable.getID() == id)
				return childTable; 
		}
		return null; 
	}	
	function getTableInIndex(index)
	{
		if(index < 0 || index > childTables.length)
			throw new Error("index is out of range"); 
			
		return childTables[index];
	}	
	return this;
}
function GridEXTableCollection(gridEX)
{
	var tables = new Array(); 
	var gridEX = gridEX; 		
	this.Count = Count; 
	this.getIndexOf = getIndexOf; 
	this.getTableByID = getTableByID;
	this.getTableInIndex = getTableInIndex; 	
	this.Add = Add;	
	function Count()
	{
		return tables.length; 
	}	
	function Add(gridEXTable)
	{
		tables[tables.length] = gridEXTable; 		
	}
	function getIndexOf(id)
	{
		var l = tables.length;
		for(var i = 0; i < l; i++)
		{
			if(tables[i].getID() == id)
				return i; 
		}
		return -1; 
	}
	function getTableByID(id)
	{		
		var _tablesLength = tables.length; 
		var table = null; 
		for(var index = 0; index < _tablesLength; index++)
		{			
			table = tables[index];
			if(table.getID() == id) 
				return table; 
		}		
		throw Error("the GridEXTable with ID '" + id + "' is not in the collection"); 
	}	
	function getTableInIndex(index)
	{
		if(index < 0 || index > tables.length) 
			throw new Error("'index' is out of range");
			
		return tables[index]; 
	}
	return this; 
}
function GridEXColumnSetCollection(gridEXTable, parentElement, isInHeader, gridEXHeader)
{
	var columnSets = new Array(); 
	var gridEXTable = gridEXTable;
	var gridEXHeader = gridEXHeader; 
	var isinHeader = isInHeader; 
	var parentElement = parentElement; 
	var fixedwidth = 0; 	
	this.getColumnSetInIndex = getColumnSetInIndex;	
	this.getColumnSetsCoreWidth = getColumnSetsCoreWidth; 
	this.getColumnSetsWidth = getColumnSetsWidth; 
	this.getCount = getCount; 	
	this.getGridEXHeader = getGridEXHeader; 
	this.getGridEXTable = getGridEXTable;
	this.getGridEX = getGridEX; 	
	this.Add = Add;	
	this.AutoSizeColumns = AutoSizeColumns; 	
	this.AutoSizeColumnsByHeader = AutoSizeColumnsByHeader; 
	this.AutoSizeByColumnSet = AutoSizeByColumnSet; 	
	this.updateColumnsDefinition = updateColumnsDefinition;
	function Add(columnSet)
	{						
		columnSets[columnSets.length] = columnSet;		
	}	
	function AutoSizeColumns(headers)
	{
		if(isinHeader)
			AutoSizeByHeaders(headers);		
	}	
	function AutoSizeColumnsByHeader(_header)
	{	
		if(columnSets.length > 0)
		{
			var columnSetsLength = columnSets.length; 			
			var innerColumnSet = null; 			
			var _innerColumnSet = null; 
			var cell = null; 
			var _cell = null; 
			var _cellsLength = -1; 
			for(var iColumnSet = 0; iColumnSet < columnSetsLength; iColumnSet++)
			{
				_innerColumnSet   = _header.getColumnSets().getColumnSetInIndex(iColumnSet).getHtmlColumnSet(); 
				innerColumnSet = getColumnSetInIndex(iColumnSet).getHtmlColumnSet(); 
				_cellsLength = -1;
				if(_innerColumnSet.cells != null)
					_cellsLength = _innerColumnSet.cells.length; 
				else
					_cellsLength = getCellsCore(_innerColumnSet).length;
				for(var i = 0; i < _cellsLength; i++)
				{
					_cell = _innerColumnSet[i]; 
					if(_cell.getAttribute("type") != "space")
					{
						cell = innerColumnSet[i]; 
						cell.style.width = (_cell.offsetWidth) + "px"; 
						cell.childNodes[0].style.width = (_cell.childNodes[0].offsetWidth) + "px";
					}
				}				
			}
		}
	}	
	function getColumnSetInIndex(index)
	{
		if(index < 0 || index >= columnSets.length) 
			throw Error("index out of range exception");
		
		return columnSets[index]; 
	}			
	function getCount()
	{
		return columnSets.length; 
	}	
	function getGridEX()
	{
		return getGridEXTable().getGridEX(); 
	}	
	function getGridEXHeader()
	{
		return gridEXHeader; 
	}	
	function getGridEXTable()
	{
		return gridEXTable; 
	}	
	function getColumnSetsWidth()
	{		
		var columnsetswidth = 0; 
		var row = parentElement.rows[0]; 
		var _cellsLength = row.cells.length; 
		for(var icell = 0; icell < _cellsLength; icell++)
		{		
			if(row.cells[icell].getAttribute("type") != "rh")
				columnsetswidth += row.cells[icell].offsetWidth;  
		}
		return columnsetswidth; 
	}
	function getFixedColumnSetsWidth()
	{
		var w = 0;
		var r = parentElement.rows[0];
		var l = r.cells.length;
		for(var i=0;i<l;i++)
		{
			if(r.cells[i].getAttribute("type") != "rh" && r.cells[i].getAttribute("allowsize") != null)
				w += r.cells[i].offsetWidth;
		}
		return w; 
	}
	function getColumnSetsCoreWidth()
	{
		var columnsetswidth = 0; 
		var _length = columnSets.length; 
		for(var icolumnset = 0; icolumnset < _length; icolumnset++)
			columnsetswidth += columnSets[icolumnset].getHtmlColumnSet().offsetWidth; 
		return columnsetswidth; 
	}
	function AutoSizeByHeaders(headers)
	{	
		var row = null;		
		if(parentElement == null) 
			throw Error("columnsets parent can't be null");		
		
		var width = getGridEXTable().getGridEX().getResizeWidth(); 
		if(width <= 0)
			return;
			
		var oldwidth = parentElement.offsetParent.offsetWidth - fixedwidth;
		var newwidth = width - fixedwidth;		
		var originalsets = new Array(); 
		var originalcells = new Array(); 		
		row = parentElement.rows[0];		
		var _cellslength = row.cells.length; 
		var cell = null; 
		var columnset = null; 		
		for(var icell = 0; icell < _cellslength; icell++)
		{	
			cell = row.cells[icell]; 
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
				originalsets[originalsets.length] = cell.offsetWidth;				
		}		
		var igset = 0; 
		var igcell = 0;
		var borderwidth = null;
		var oldsize = null; 
		var newsize = null; 		
		var _cellIndex = null; 		
		var _htmlColumnSet = null; 
		var _sumwidth = 0; 
		var _cols = null; 
		for(var icell = 0; icell < _cellslength; icell++)
		{
			cell = row.cells[icell]; 
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
			{				
				borderwidth = getBorderWidth(cell); 
				oldsize = originalsets[igset];
				newsize = Math.round((oldsize * newwidth) / oldwidth);					
				oldsize -= borderwidth;
				newsize -= borderwidth;				
				_cellIndex = parseInt(cell.getAttribute("index"), 10); 				
				columnset = getColumnSetInIndex(_cellIndex);
				columnset.AutoSize(oldsize, newsize, igcell, originalcells, headers);				
				_htmlColumnSet = columnset.getHtmlColumnSet(); 
				_sumwidth = 0; 
				_cols = _htmlColumnSet.getElementsByTagName("COL"); 
				for(var _icol = 0; _icol < _cols.length; _icol++)
					_sumwidth += getPixelColWidth(_cols[_icol].width); 					
				_htmlColumnSet.style.width = _sumwidth + "px"; 
				igset++; 								
			}
		}		
		FixAutoSizeWidth(newwidth, headers);		
		updateColumnsDefinition();
		AutoSizeItems();		
	}	
	function AutoSizeByColumnSet(columnset, remainoldwidth, remainnewwidth, currwidth)
	{	
		var _headers = getGridEXTable().getHeaders(); 
		var _inallHeaders = _headers.length > 1; 		
		var _columnSetsLength = columnSets.length;
		var acolumnset = null; 
		var borderwidth = null; 
		var igcell = 0; 
		var originalwidth = new Array();
		var newwidth = null; 
		var _minimalWidth = getMinimalColumnSetsWidth(columnSets[columnset]); 
		var _sumwidth = 0; 
		for(var index = 0; index < _columnSetsLength; index++)
		{
			acolumnset = columnSets[index]; 
			if(columnset != acolumnset.getIndex())
			{
				if(acolumnset.getHtmlColumnSet().parentElement.getAttribute("allowsize") == null)
					originalwidth[originalwidth.length] = acolumnset.getHtmlColumnSet().parentElement.offsetWidth;
			}
		}
		remainoldwidth -= fixedwidth; 
		remainnewwidth -= fixedwidth; 
		for(var index = 0; index < _columnSetsLength; index++)
		{
			acolumnset = columnSets[index]; 
			if(columnset != acolumnset.getIndex())
			{	
				if(acolumnset.getHtmlColumnSet().parentElement.getAttribute("allowsize") == null)
				{
					borderwidth = getBorderWidth(acolumnset.getHtmlColumnSet().parentElement);
					newwidth = Math.round((originalwidth[igcell] * remainnewwidth) / remainoldwidth);
					newwidth -= borderwidth;
					if(newwidth < _minimalWidth[index])
						newwidth = _minimalWidth[index];				
					
					if(_inallHeaders)
						acolumnset.ResizeColumnSet(newwidth, _headers);
					else
						acolumnset.ResizeColumnSet(newwidth, null); 
					igcell++;
				}
			}			
		}		
		FixAutoSizeWidth(currwidth - fixedwidth, _inallHeaders ? _headers : null); 
		updateColumnsDefinition(); 
		AutoSizeItems();
	}
	function cellCouldResizeOthers(cell, tableCells)
	{
		var cellsLength = -1;
		if(tableCells.cells != null)
			cellsLength = tableCells.cells.length; 
		else
			cellsLength = getCellsCore(tableCells).length;
		var _cell = null; 
		if(tableCells.cells != null)
			_cell = tableCells.cells[cell]; 
		else
			_cell = getCellsCore(tableCells)[cell];
		var _low = parseInt(_cell.getAttribute("usecol"), 10); 
		var _high = _low + _cell.colSpan; 	
		for(var icell=0;icell<cellsLength;icell++)
		{
			if(icell != cell)
			{
				if(tableCells.cells != null)
					_cell = tableCells.cells[icell]; 
				else
					_cell = getCellsCore(tableCells)[icell];
				if(_cell.getAttribute("type") != "space" && _cell.getAttribute("type") != "header")
				{	
					usecol = parseInt(_cell.getAttribute("usecol"), 10); 
					if(usecol >= _low && usecol < _high)
					{
						if(_cell.getAttribute("allowsize") != null)
							return false; 
					}
				}
			}
		}
		return true;
	}
	function refreshColumnSetsSize(row, _cellsLength)
	{
		var cell = null;
		for(var index = 0; index < _cellsLength; index++)
		{
			cell = row.cells[index]; 
			if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
			{
				_columnsetIndex = parseInt(cell.getAttribute("index"), 10); 
				columnset = getColumnSetInIndex(_columnsetIndex);
				innercolumnset = columnset.getHtmlColumnSet(); 
				_cols = innercolumnset.getElementsByTagName("COL"); 
				_sumwidth = 0; 
				for(var _icol = 0; _icol < _cols.length; _icol++)
					_sumwidth += getPixelColWidth(_cols[_icol].width); 
				
				innercolumnset.style.width = _sumwidth + "px"; 
			}			
		}
	}
	function FixAutoSizeWidth(width, headers)
	{
		var row = parentElement.rows[0]; 
		var columnsetswidth = getColumnSetsCoreWidth();
		var diffsize = width - columnsetswidth;  
		var offset; 
		if(diffsize < 0) 
			offset = -1;
		else if(diffsize > 0) 
			offset = 1;
		else
		{
			refreshColumnSetsSize(row, row.cells.length);
			return; 
		}			
		var _cellwidth  = 0; 
		var _diff = 0; 
		var _cellsLength = row.cells.length; 
		var _innercellsLength = null; 
		var cell = null; 
		var columnset = null; 
		var innercell = null; 
		var innercolumnset = null;
		var _cell = null; 
		var _col = null; 
		var _columnset = null; 
		var _columnsetIndex = null; 
		var _lowwidth = 0; 
		var _header = null; 		
		var _headersLength = (headers != null) ? headers.length : -1; 
		var _cols = null;
		var _childcols = null; 
		var _childcol = null;  
		var olddiff = -1; 
		var colspan = -1; 
		var _fixedwidth = -1;
		var trycount = 0;
		var _sumwidth = 0; 
		do
		{	
			olddiff = diffsize;
			for(var i = 0; i < _cellsLength && diffsize != 0 && trycount < 3; i++)
			{
				cell = row.cells[i];
				if(cell.getAttribute("type") != "rh" && cell.getAttribute("allowsize") == null)
				{
					_columnsetIndex = parseInt(cell.getAttribute("index"), 10); 
					columnset = getColumnSetInIndex(_columnsetIndex);
					innercolumnset = columnset.getHtmlColumnSet(); 
					_innercellsLength = -1;
					if(innercolumnset.cells != null)
						_innercellsLength = innercolumnset.cells.length; 
					else
						_innercellsLength = getCellsCore(innercolumnset).length;
					_cols = innercolumnset.getElementsByTagName("COL");
					var fixedcols = new Array(_cols.length);
					for(var icell = 0; icell < _innercellsLength; icell++)
					{
						innercell = null;
						if(innercolumnset.cells != null)
							innercell = innercolumnset.cells[icell];						
						else
							innercell = getCellsCore(innercolumnset)[icell];
						if(innercell.getAttribute("type") != "space" && innercell.getAttribute("allowsize") == null)
						{
							if(innercell.colSpan != columnset.getColumnCount() || cellCouldResizeOthers(icell, innercolumnset, _innercellsLength))
							{
								if(diffsize != 0)
								{
									if(innercell.getAttribute("type") != null && innercell.getAttribute("type") == "ch")
										_lowwidth = getGridEXTable().getHeaderWidth();
									else
									{
										if(innercell.getAttribute("pec") != null)
											_lowwidth = 18;
										else
											_lowwidth = 0; 
									}
									var usecol = parseInt(innercell.getAttribute("usecol"), 10); 
									colspan = innercell.colSpan + usecol; 
									_fixedwidth = 0; 
									for(var icol = usecol; icol < colspan; icol++)
									{
										if(fixedcols[icol] == null)
										{
											if(diffsize != 0)
											{
												if(getPixelColWidth(_cols[icol].width) + offset > _lowwidth)
												{
													_cols[icol].width = (getPixelColWidth(_cols[icol].width) + offset) + "px"; 
													columnsetswidth += offset; // getColumnSetsCoreWidth();						
													diffsize = width  - columnsetswidth;
												}
												fixedcols[icol] = 1; 
											}
										}
										_fixedwidth += getPixelColWidth(_cols[icol].width);
									}
									var diff = getPaddingLeft(innercell) + getPaddingRight(innercell) + getBorderWidth(innercell) + getSortWidth(innercell);
									if(_fixedwidth > _lowwidth)
										innercell.childNodes[0].style.width = _fixedwidth + "px"; 
									if(_fixedwidth - diff > _lowwidth)
										getInnerSpan(innercell.childNodes[0]).style.width = (_fixedwidth - diff) + "px"; 
								}
								else
								{
									colspan = innercell.colSpan + parseInt(innercell.getAttribute("usecol"), 10); 
									_fixedwidth = 0; 
									for(var icol = parseInt(innercell.getAttribute("usecol"), 10); icol < colspan; icol++)
										_fixedwidth += getPixelColWidth(_cols[icol].width); 
									var diff = getPaddingLeft(innercell) + getPaddingRight(innercell) + getBorderWidth(innercell) + getSortWidth(innercell);
									if(_fixedwidth > _lowwidth)
										innercell.childNodes[0].style.width = _fixedwidth + "px";
									if(_fixedwidth - diff > _lowwidth)
										getInnerSpan(innercell.childNodes[0]).style.width = (_fixedwidth - diff) + "px";
								}
							}
						}
						else if(innercell.getAttribute("allowsize") != null)
						{
							if(width <= innercell.offsetWidth)
								return; 
						}
					}
				}
			}
			if(olddiff == diffsize)
				trycount++; 
		} while(diffsize != 0 && trycount < 3);		
		refreshColumnSetsSize(row, _cellsLength); 
	}	
	function AutoSizeItems(columnsWidth)
	{
		var _columnSetsLength = columnSets.length; 
		var columnset = null; 
		for(var index = 0; index < _columnSetsLength; index++)
		{
			columnset = getColumnSetInIndex(index);
			columnset.AutoSizeItems(columnsWidth);
		}
	}	
	function updateColumnsDefinition()
	{		
		var _field = document.getElementsByName(getGridEXTable().getID() + "_cols")[0];
		if(_field == null)
			throw Error("input field for columns definition is null");					
		
		var cell = null;		
		var columnSetsLength = columnSets.length; 
		var columnset = null;
		var innercolumnset = null;		
		var _cols = null; 
		var _length = null;
		for(var icolumnset = 0; icolumnset < columnSetsLength; icolumnset++)	
		{			
			columnset = columnSets[icolumnset]; 
			innercolumnset = columnset.getHtmlColumnSet();	
			if(innercolumnset.cells != null)
				_length = innercolumnset.cells.length; 
			else
				_length = getCellsCore(innercolumnset).length;
			_cols = innercolumnset.getElementsByTagName("COL"); 
			for(var index = 0; index < _length; index++)
			{			
				if(innercolumnset.cells != null)
					cell = innercolumnset.cells[index]; 
				else
					cell = getCellsCore(innercolumnset)[index]; 
				if(cell.getAttribute("allowsize") == null)
				{						
					if((cell.getAttribute("type") != "header" && cell.getAttribute("type") != "space") && (cell.id != null && cell.getAttribute("usecol") != null))	
					{
						_width = getPixelColWidth(_cols[parseInt(cell.getAttribute("usecol"), 10)].width); 
						if(cell.getAttribute("type") != null && cell.getAttribute("type") == "ch")
							_width -= getGridEXTable().getHeaderWidth();
						
						updateColumnDefinitionInField(_field, cell.id, -1,  _width, cell.childNodes[0].offsetWidth);
					}
				}
			}
		}
	}		
	var row = parentElement.rows[0];
	var _cellsLength = row.cells.length; 
	for(var icell = 0; icell < _cellsLength; icell++)
	{
		var cell = row.cells[icell];
		if(cell.getAttribute("type") == "rh" || cell.getAttribute("allowsize") != null)
			fixedwidth += cell.offsetWidth; 
	}
	return this;
}
function GridEXColumnSet(gridEXTable, index, htmlTable, isInHeader, gridEXHeader)
{		
	var gridEXTable = gridEXTable; 	
	var gridEXHeader = gridEXHeader; 
	var htmlTable = htmlTable;
	var index = index; 
	var isinHeader = isInHeader;		
	this.getColumnCount = getColumnCount; 
	this.getGridEXHeader = getGridEXHeader; 
	this.getGridEXTable = getGridEXTable; 
	this.getGridEX = getGridEX; 
	this.getHtmlColumnSet = getHtmlColumnSet; 
	this.getHtmlColumnByID = getHtmlColumnByID; 
	this.getIndex = getIndex; 
	this.getIsInHeader = getIsInHeader;	
	this.AutoSizeColumn = AutoSizeColumn;
	this.AutoSizeColumns = AutoSizeColumns; 	
	this.AutoSize = AutoSize; 	
	this.AutoSizeItems = AutoSizeItems; 
	this.CopyCellsWidth = CopyCellsWidth;
	this.ResizeColumnSet = ResizeColumnSet; 
	this.ResizeColumnSetHeader = ResizeColumnSetHeader;
	function getColumnCount()
	{		
		if(htmlTable.getAttribute("cc") != null)
			return parseInt(htmlTable.getAttribute("cc"), 10);
		else
			return 0; 
	}	
	function getGridEXHeader() { return gridEXHeader;  }	
	function getGridEX() { return getGridEXTable().getGridEX(); }	
	function getGridEXTable() { return gridEXTable;  }	
	function getHtmlColumnSet() { return htmlTable; }
	function getHtmlColumnByID(colID)
	{
		if(htmlTable == null)
			throw Error("invalid operation exception"); 
			
		return htmlTable.all.item(colID); 
	}
	function getIndex() { return index; }	
	function getIsInHeader() { return isinHeader; }
	function ResizeColumnSet(newwidth, headers)
	{		
		var _cellsLength = null; 	
		var _cols = null; 	
		var _colslength = null; 		
		var cell = null;
		var diff = null; 		
		var newcellsize = null; 
		var oldwidth = htmlTable.offsetWidth;
		if(htmlTable.cells != null)		
			_cellsLength = htmlTable.cells.length;
		else
			_cellsLength = getCellsCore(htmlTable).length;
		_cols = htmlTable.getElementsByTagName("COL");		
		var colspan = 0; 
		var _fixedwidth = null; 
		for(var icell = 0; icell < _cellsLength; icell++)
		{			
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("allowsize") != null && cell.getAttribute("usecol") != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
				_fixedwidth[cell.getAttribute("usecol")] = cell.offsetWidth; 
			}
		}
		if(_fixedwidth != null)
		{
			for(var i = 0; i < _fixedwidth.length; i++)
			{				
				if(_fixedwidth[i] != null)
				{
					newwidth -= _fixedwidth[i]; 
					oldwidth -= _fixedwidth[i]; 
				}
			}
		}
		var _fixedcols = new Array(_cols.length);
		var _oldcols = new Array(_cols.length); 
		for(var icol = 0; icol < _cols.length; icol++)
			_oldcols[icol] = getPixelColWidth(_cols[icol].width);	
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell]; 
			if(cell.getAttribute("type") != "space" && cell.getAttribute("allowsize")  == null)
			{
				if(cell.getAttribute("type") == "header" || cell.colSpan == getColumnCount())
				{								
					diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);		
					if(newwidth - diff > 0)
					{
						if(cell.getAttribute("type") == "header")
						{
							cell.style.width = newwidth + "px"; 
							cell.childNodes[0].style.width = newwidth + "px"; 
							getInnerSpan(cell.childNodes[0]).style.width = (newwidth - diff) + "px"; 
						}
						else
						{
							_fixedwidth = 0; 
							colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10); 							
							for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
							{
								if(_fixedcols[icol] == null)
								{
									if(Math.round((_oldcols[icol] * newwidth) / oldwidth) >= 1)
									{
										_cols[icol].width = Math.round((_oldcols[icol] * newwidth) / oldwidth) + "px"; 
										_fixedcols[icol] = 1; 
									}
								}
								_fixedwidth += getPixelColWidth(_cols[icol].width);
							}
							if(_fixedwidth - diff > 0)
							{
								cell.childNodes[0].style.width = (_fixedwidth) + "px"; 
								getInnerSpan(cell.childNodes[0]).style.width = (_fixedwidth - diff) + "px"; 
							}
						}
					}
					if(headers != null)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols);
				}
				else
				{	
					_fixedwidth = 0; 
					colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10);
					for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
					{
						if(_fixedcols[icol] == null)
						{
							if(Math.round((_oldcols[icol] * newwidth) / oldwidth) >= 1)
							{
								_cols[icol].width = Math.round((_oldcols[icol] * newwidth) / oldwidth) + "px"; 
								_fixedcols[icol] = 1; 
							}
						}
						_fixedwidth += getPixelColWidth(_cols[icol].width);
					}
					diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);
					if(_fixedwidth > 0)
						cell.childNodes[0].style.width = _fixedwidth + "px"; 
					if(_fixedwidth - diff > 0)
						getInnerSpan(cell.childNodes[0]).style.width = (_fixedwidth - diff) + "px"; 
					if(headers != null)
						ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 					
				}			
			}
		}
		var _sumwidth = 0; 
		for(var _icol = 0; _icol < _cols.length; _icol++)
			_sumwidth += getPixelColWidth(_cols[_icol].width); 
		htmlTable.style.width = _sumwidth + "px"; 
	}	
	function ResizeColumnSetHeader(column, posX)
	{			
		var offsetwidth = -1;		
		posX += getScrollLeft(getGridEXTable().getGridEX()); 
		offsetwidth = posX -  (htmlTable.offsetWidth + getPixelLeft(htmlTable));
		if(!getGridEXHeader().getIsRoot())
			offsetwidth += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;			
		if(offsetwidth  > 0) 
		{						
			if(posX < getGridEXTable().getGridEX().getHtmlGridEX().offsetWidth) 
			{				
				var oldsize = htmlTable.offsetWidth;
				var newsize = oldsize + offsetwidth;
				var oldcolumnsetwidth = htmlTable.parentElement.offsetWidth; 
				var columnsetswidth = getGridEXHeader().getColumnSets().getColumnSetsWidth();  								
				AutoSizeColumns(newsize, oldsize);				
				if(getGridEX().getColumnAutoResize())
					getGridEXHeader().getColumnSets().AutoSizeByColumnSet(getIndex(), columnsetswidth - oldcolumnsetwidth, columnsetswidth - htmlTable.parentElement.offsetWidth, columnsetswidth);				
				else
					AutoSizeItems(); 
			}
		}
		else 
		{										
			if((column.getAttribute("type") != null && column.getAttribute("type") == "header") || column.colSpan == getColumnCount() || isMostRight(column))
			{
				var oldsize = htmlTable.offsetWidth;
				var newsize = oldsize - (htmlTable.offsetLeft + htmlTable.offsetWidth - posX); 
				if(newsize < oldsize && getGridEX().getColumnAutoResize() && getGridEXHeader().getColumnSets().getCount() == 1)
					return; 
					
				var oldcolumnsetwidth = htmlTable.parentElement.offsetWidth;
				var columnsetswidth = getGridEXHeader().getColumnSets().getColumnSetsWidth(); 									
				AutoSizeColumns(newsize, oldsize);				
				if(getGridEX().getColumnAutoResize())		
					getGridEXHeader().getColumnSets().AutoSizeByColumnSet(getIndex(), columnsetswidth - oldcolumnsetwidth, columnsetswidth - htmlTable.parentElement.offsetWidth, columnsetswidth);					
				else
					AutoSizeItems(); 
			}
			else
			{				
				var oldsize = column.offsetWidth; 
				var cellsize = oldsize - (column.offsetWidth + getPixelLeft(column) - posX);  
				offsetwidth = cellsize - oldsize;					
				var oldwidth = htmlTable.offsetWidth;					
				var newwidth = -1;
				if(getGridEX().getColumnAutoResize())
					newwidth = oldwidth; 
				else
					newwidth = htmlTable.offsetWidth + offsetwidth;
				var oldremain = oldwidth - oldsize;
				var newremain = oldwidth - cellsize;
				ResizeCellsInSet(column, cellsize, oldsize, oldremain, newremain);						
				AutoSizeItems();
				getGridEXHeader().getColumnSets().updateColumnsDefinition(); 
			}
		}				
	}	
	function ResizeColumnInColumnSetHeaders(column, columnIndex, columnSetIndex, cols)
	{		
		var _arrHeaders = getGridEXTable().getHeaders();
		var _cell = null; 
		var _columnSet = null;
		var _cols = null; 
		var _headerIndex = getGridEXHeader().getIndex();
		var _htmlColumnSet = null;		
		for(var iheader = 0; iheader < _arrHeaders.length; iheader++)
		{			
			if(iheader != _headerIndex)
			{
				_columnSet = _arrHeaders[iheader].getColumnSets().getColumnSetInIndex(columnSetIndex);
				_htmlColumnSet = _columnSet.getHtmlColumnSet(); 	
				_cols = _htmlColumnSet.getElementsByTagName("COL"); 		
				if(_htmlColumnSet.cells != null)					
					_cell = _htmlColumnSet.cells[columnIndex];				
				else
					_cell = getCellsCore(_htmlColumnSet)[columnIndex];
				if((column.getAttribute("type") == "ch" || column.getAttribute("isCH") != null) && (_cell.getAttribute("type") != "ch" && _cell.getAttribute("isCH") == null))
				{	
					var _width = getPixelColWidth(cols[parseInt(column.getAttribute("usecol"), 10)].width) - getGridEXTable().getHeaderWidth();
					if(_cell.getAttribute("pec") != null && _cell.getAttribute("type") != "ch")
						_width += 18;											
					_cols[parseInt(_cell.getAttribute("usecol"), 10)].width = _width + "px";
					if(_cell.colSpan > 1)
					{
						var colspan = _cell.colSpan + parseInt(_cell.getAttribute("usecol"), 10); 
						for(var icol = parseInt(_cell.getAttribute("usecol"), 10) + 1; icol < colspan; icol++)
						{
							_cols[icol].width = cols[icol].width; 
							_width += getPixelColWidth(_cols[icol].width); 
						}
					}				
					var diff = getPaddingLeft(_cell) + getPaddingRight(_cell) + getBorderWidth(_cell) + getSortWidth(_cell);
					_cell.childNodes[0].style.width = _width + "px"; 
					getInnerSpan(_cell.childNodes[0]).style.width = (_width - diff) + "px";					
				}
				else if((column.getAttribute("type") != "ch" && column.getAttribute("isCH") == null) && (_cell.getAttribute("type") == "ch" || _cell.getAttribute("isCH") != null))
				{				
					var _width = getPixelColWidth(cols[parseInt(column.getAttribute("usecol"), 10)].width) + getGridEXTable().getHeaderWidth(); 
					if(_cell.getAttribute("pec") != null && _cell.getAttribute("type") != "ch")
						_width += 18;
					_cols[parseInt(_cell.getAttribute("usecol"), 10)].width = _width + "px"; 	
					if(_cell.colSpan > 1)
					{						
						var colspan = _cell.colSpan + parseInt(_cell.getAttribute("usecol"), 10); 
						for(var icol = parseInt(_cell.getAttribute("usecol"), 10) + 1; icol < colspan; icol++)
						{
							_cols[icol].width = cols[icol].width; 
							_width += getPixelColWidth(_cols[icol].width); 
						}
					}
					var diff = getPaddingLeft(_cell) + getPaddingRight(_cell) + getBorderWidth(_cell) + getSortWidth(_cell);
					_cell.childNodes[0].style.width = _width + "px"; 
					getInnerSpan(_cell.childNodes[0]).style.width = (_width - diff) + "px";
				}
				else
				{
					var colspan = _cell.colSpan + parseInt(_cell.getAttribute("usecol"), 10); 
					var _width = 0; 
					for(var icol = parseInt(_cell.getAttribute("usecol"), 10); icol < colspan; icol++)
					{
						_cols[icol].width = cols[icol].width; 
						_width += getPixelColWidth(_cols[icol].width); 
					}
					var diff = getPaddingLeft(_cell) + getPaddingRight(_cell) + getBorderWidth(_cell) + getSortWidth(_cell);
					_cell.childNodes[0].style.width = _width + "px"; 
					getInnerSpan(_cell.childNodes[0]).style.width = (_width - diff) + "px"; 					
				}
			}
		}
	}
	function cellCouldResizeOthers(cell, cellsLength)
	{		
		var _cell = null;
		if(htmlTable.cells != null)
			_cell = htmlTable.cells[cell]; 
		else
			_cell = getCellsCore(htmlTable)[cell]; 
		var _low = parseInt(_cell.getAttribute("usecol"), 10); 
		var _high = _low + _cell.colSpan; 	
		for(var icell=0;icell<cellsLength;icell++)
		{
			if(icell != cell)
			{
				if(htmlTable.cells != null)					
					_cell = htmlTable.cells[icell]; 
				else
					_cell = getCellsCore(htmlTable)[icell];
				if(_cell.getAttribute("type") != "space" && _cell.getAttribute("type") != "header")
				{	
					usecol = parseInt(_cell.getAttribute("usecol"), 10); 
					if(usecol >= _low && usecol < _high)
					{
						if(_cell.getAttribute("allowsize") != null)
							return false; 
					}
				}
			}
		}
		return true; 
	}
	function AutoSizeColumns(newsize, oldsize)
	{		
		var cell = null;		
		var diff = null; 		
		var _cellsLength = -1;
		if(htmlTable.cells != null)
			_cellsLength = htmlTable.cells.length;				
		else
			_cellsLength = getCellsCore(htmlTable).length;
		var _cols = htmlTable.getElementsByTagName("COL");
		var colswidth = new Array(_cols.length); 
		var inallheaders = getGridEXTable().getHeaders().length > 1; 				
		var _fixedwidth = null; 
		for(var icell = 0; icell < _cellsLength; icell++)
		{			
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("allowsize") != null && cell.getAttribute("usecol") != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
				_fixedwidth[cell.getAttribute("usecol")] = cell.offsetWidth; 
			}
		}
		for(var icol = 0; icol < _cols.length; icol++)
			colswidth[icol] = getPixelColWidth(_cols[icol].width);
		if(_fixedwidth != null)
		{
			for(var i = 0; i < _fixedwidth.length; i++)
			{				
				if(_fixedwidth[i] != null)
				{
					newsize -= _fixedwidth[i]; 
					oldsize -= _fixedwidth[i]; 
				}
			}
		}
		var _fixedcols = new Array(_cols.length); 		
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell]; 
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("type") != "space" && cell.getAttribute("allowsize") == null)
			{
				if(cell.getAttribute("type") == "header" || cell.colSpan == getColumnCount())
				{				
					diff = getPaddingLeft(cell) + getPaddingRight(cell)  + getBorderWidth(cell) + getSortWidth(cell);
					if(cell.getAttribute("type") == "header")
					{
						cell.style.width = newsize + "px";
						cell.childNodes[0].style.width = newsize + "px";
						getInnerSpan(cell.childNodes[0]).style.width = (newsize - diff) + "px";
					}
					else if(cellCouldResizeOthers(icell, _cellsLength))
					{
						var colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10); 
						var _fixedwidth = 0; 
						for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
						{
							if(_fixedcols[icol] == null)
							{
								if(Math.round((colswidth[icol] * newsize) / oldsize) >= 1)
								{
									_cols[icol].width = Math.round((colswidth[icol] * newsize) / oldsize) + "px"; 
									_fixedcols[icol] = 1; 
								}
							}
							_fixedwidth += getPixelColWidth(_cols[icol].width);
						}
						if(_fixedwidth > 0)
						{
							cell.style.width = _fixedwidth + "px"; 
							cell.childNodes[0].style.width = _fixedwidth + "px"; 
						}
						if(_fixedwidth - diff > 0)
							getInnerSpan(cell.childNodes[0]).style.width = (_fixedwidth - diff) + "px"; 
						if(inallheaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 
					}
				}
				else
				{	
					var colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10); 
					var _fixedwidth = 0; 
					for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
					{
						if(_fixedcols[icol] == null)
						{
							if(Math.round((colswidth[icol] * newsize) / oldsize) >= 1)
							{
								_cols[icol].width = Math.round((colswidth[icol] * newsize) / oldsize) + "px"; 
								_fixedcols[icol] = 1; 
							}
						}
						_fixedwidth += getPixelColWidth(_cols[icol].width);
					}		
					diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
					if(_fixedwidth > 0)
					{
						cell.style.width = _fixedwidth + "px"; 
						cell.childNodes[0].style.width = _fixedwidth + "px"; 
					}
					if(_fixedwidth - diff > 0)
						getInnerSpan(cell.childNodes[0]).style.width = (_fixedwidth - diff) + "px"; 					
					if(inallheaders)
						ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols);
				}
			}			
		}
	}	
	function AutoSizeColumn(column, htmlColumn)
	{
		htmlColumn.style.cursor = "default"; 			
		var columnsetswidth = getGridEXHeader().getColumnSets().getColumnSetsCoreWidth();
		var oldColumnSetWidth = htmlTable.offsetWidth; 		
		var oldColumnSize = htmlColumn.offsetWidth;
		var maxColumnSize = getMaximumColumnSize(column);
		if(maxColumnSize <= 0)
			return; 
			
		if(htmlColumn.type == "ch") 
			maxColumnSize += getGridEXTable().getHeaderWidth();
			
		var offset = (maxColumnSize - oldColumnSize); 
		var newColumnSetWidth = oldColumnSetWidth + offset; 		
		if(htmlColumn.colSpan == getColumnCount())
		{
			AutoSizeColumns(maxColumnSize, oldColumnSetWidth); 
			if(getGridEX().getColumnAutoResize())
				getGridEXHeader().getColumnSets().AutoSizeByColumnSet(getIndex(), columnsetswidth - oldColumnSetWidth, Math.abs(columnsetswidth - htmlTable.parentElement.offsetWidth), columnsetswidth); 
			else
				AutoSizeItems(); 
		}
		else
		{
			ResizeCellsInSet(htmlColumn, maxColumnSize, oldColumnSize, oldColumnSetWidth - oldColumnSize, newColumnSetWidth - maxColumnSize); 
			if(getGridEX().getColumnAutoResize())
				getGridEXHeader().getColumnSets().AutoSizeByColumnSet(getIndex(), columnsetswidth - oldColumnSetWidth, columnsetswidth - htmlTable.parentElement.offsetWidth, columnsetswidth); 								
			else
				AutoSizeItems(); 
		}
	}
	function AutoSize(oldsize, newsize, igcell, cells, headers)	
	{	
		var _cellsLength = -1;
		if(htmlTable.cells != null)
			_cellsLength = htmlTable.cells.length;		
		else
			_cellsLength = getCellsCore(htmlTable).length;
		var _cols = htmlTable.getElementsByTagName("COL");
		var cell = null; 
		var cellwidth = null; 
		var diff = null;		
		var _oldwidth = new Array();
		var _fixedwidth = null; 					
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("type") != "space" && cell.getAttribute("type") != "header" && cell.getAttribute("allowsize") == null)
				_oldwidth[_oldwidth.length] = cell.offsetWidth;
			else if(cell.getAttribute("allowsize") != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
					
				_fixedwidth[cell.getAttribute("usecol")] = cell.offsetWidth; 
			}
		}	
		var _oldcols = new Array(_cols.length); 
		for(var icol = 0; icol < _cols.length; icol++)
			_oldcols[icol] = getPixelColWidth(_cols[icol].width);	
		if(_fixedwidth != null)
		{
			for(var i = 0; i < _fixedwidth.length; i++)
			{
				if(_fixedwidth[i] != null)
				{
					oldsize -= _fixedwidth[i];
					newsize -= _fixedwidth[i]; 
				}
			}
		}
		if(oldsize <= 0 || newsize <= 0)
			return;
			
		var _icell = 0; 
		var _fixedcols = new Array(_cols.length);
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell]; 
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("type") != "space" && cell.getAttribute("allowsize") == null)
			{
				if(cell.getAttribute("type") == "header")
					cellwidth = newsize;
				else
				{
					cellwidth = Math.round((_oldwidth[_icell] * newsize) / oldsize); 
					_icell++;
				}
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);
				if(cellwidth - diff > 0)
				{					
					if(cell.getAttribute("type") == "header")
					{
						cell.childNodes[0].style.width = (cellwidth - diff) + "px"; 
						cell.style.width = (cellwidth - diff) + "px";
					}
					else
					{
						if(cell.colSpan != getColumnCount() || cellCouldResizeOthers(icell, _cellsLength))
						{
							if(cell.getAttribute("usecol") != null)
							{
								var colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10); 
								var _fixedwidth = 0; 
								for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
								{
									if(_fixedcols[icol] == null)
									{
										if(Math.round((_oldcols[icol] * newsize) / oldsize) >= 1)
										{
											_cols[icol].width = Math.round((_oldcols[icol] * newsize) / oldsize) + "px"; 
											_fixedcols[icol] = 1; 
										}
									}
									_fixedwidth += getPixelColWidth(_cols[icol].width);
								}
								if(_fixedwidth > 0)
								{
									cell.style.width = _fixedwidth + "px"; 
									cell.childNodes[0].style.width = _fixedwidth + "px";								
								}
								if(_fixedwidth - diff > 0)
									getInnerSpan(cell.childNodes[0]).style.width = (_fixedwidth - diff) + "px";
							}
							if(headers != null)
								ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols);
						}
					}
				}
			}
		}
	}
	function AutoSizeItems()
	{	
		var _cell = null; 
		var _colswidth = new Array(); 
		var _cols = htmlTable.getElementsByTagName("COL");  
		var _colsLength = _cols.length; 
		for(var i = 0; i < _colsLength; i++)
			_colswidth[_colswidth.length] = getPixelColWidth(_cols[i].width); 
			
		var _cellsLength = -1;
		if(htmlTable.cells != null)
			_cellsLength = htmlTable.cells.length; 
		else
			_cellsLength = getCellsCore(htmlTable).length;
		var _checkedCols = new Array(_cellsLength); 
		for(var _icell = 0; _icell < _cellsLength; _icell++)
		{
			if(htmlTable.cells != null)
				_cell = htmlTable.cells[_icell]; 
			else
				_cell = getCellsCore(htmlTable)[_icell];
			if(_cell.getAttribute("type") == "ch" && _cell.getAttribute("usecol") != null)
			{
				var _usecol = parseInt(_cell.getAttribute("usecol"), 10); 
				if(_checkedCols[_usecol] == null)
				{
					_colswidth[_usecol] -= getGridEXTable().getHeaderWidth(); 
					_checkedCols[_usecol] = -1; 
				}
			}
		}
		var offset = 0; 
		var _itemCol = null; 
		var _itemsCols = null;
		var _itemsTables = null;
		if(document.getChildsById != null)
			_itemsTables = document.getChildsById(getGridEXTable().getID() + "_items_cs" + getIndex()); 
		else
			_itemsTables = allChildsCore(getGridEXTable().getID() + "_items_cs" + getIndex(), document.getElementsByTagName("*")); 
		var _itemsTablesLength  = _itemsTables.length; 
		var _sumwidth = 0; 
		if(_itemsTables.length > 0)
		{			
			for(var _item = 0; _item < _itemsTablesLength; _item++)
			{
				_itemsCols = _itemsTables[_item].getElementsByTagName("COL"); 
				_sumwidth = 0; 
				for(var _icol = 0; _icol < _colsLength; _icol++)
				{
					_itemsCols[_icol].width = (_colswidth[_icol]) + "px";
					_sumwidth += _colswidth[_icol]; 
				}				
				_itemsTables[_item].style.width = _sumwidth + "px";
			}
		}
		var _newdiv = null;
		if(document.getChildsById != null)
			_newdiv = document.getChildsById("nrsep" + getGridEXTable().getID()); 
		else
			_newdiv = allChildsCore("nrsep" + getGridEXTable().getID(), document.getElementsByTagName("*"));
		if(_newdiv != null)
		{
			for(var i = 0; i < _newdiv.length; i++)
				_newdiv[i].style.width = getGridEXTable().getWidth() + "px"; 
		}
		var thdiv = null;
		if(document.getChildsById != null)
			thdiv = document.getChildsById("th" + getGridEXTable().getID());
		else
			thdiv = allChildsCore("th" + getGridEXTable().getID(), document.getElementsByTagName("*")); 
		if(thdiv != null)
		{
			for(var i=0;i<thdiv.length;i++)
			{
				var thwidth = getGridEXTable().getWidth();
				if(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset") != null)
					thwidth -= parseInt(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset"), 10);	
				thdiv[i].getElementsByTagName("COL")[0].width = thwidth + "px";
				thdiv[i].style.width = thwidth + "px"; 				
			}
		}
		if(getGridEXTable().getParent() == null)
		{
			var _htmlitemstable = getGridEXTable().getHtmlItemsTable(); 
			if(_htmlitemstable.getAttribute("empty") != null)
				_htmlitemstable.style.width = getGridEXTable().getWidth() + "px"; 
		}		
		 _sumwidth = 0; 
		for(var _icol = 0; _icol < _colsLength; _icol++)
			_sumwidth += getPixelColWidth(_cols[_icol].width);
			
		htmlTable.style.width = _sumwidth + "px";
		if(getGridEXTable().getGridEX().getResizeGroups())
		{
			if(document.getChildsById != null)
				_itemsTables = document.getChildsById("group" + getGridEXTable().getID()); 
			else
				_itemsTables = allChildsCore("group" + getGridEXTable().getID(), document.getElementsByTagName("*")); 
			_itemsTablesLength = _itemsTables.length; 
			if(_itemsTablesLength > 0)
			{
				var _tableWidth = getGridEXTable().getWidth(); 				
				for(var _item = 0; _item < _itemsTables.length; _item++)
				{
					offset = 0;
					_itemsCols = _itemsTables[_item].getElementsByTagName("COL"); 
					_itemCol = _itemsCols[0]; 
					if(_itemCol.getAttribute("offset") != null)
						offset = parseInt(_itemCol.getAttribute("offset"), 10);  
					_itemCol.width = (_tableWidth - offset)+ "px";
					_itemsTables[_item].style.width = (_tableWidth - offset) + "px"; 
					var _offsetTable = _itemsTables[_item].offsetParent; 
					var _offsetWidth = 0; 
					var _offsetCols = _offsetTable.getElementsByTagName("COL"); 
					_offsetCols[_offsetCols.length-2].width = (_tableWidth - offset) + "px"; 
					for(var i = 0; i < _offsetCols.length-1; i++)
						_offsetWidth += getPixelColWidth(_offsetCols[i].width); 
					_offsetTable.style.width = _offsetWidth + "px"; 
				}
			}
		}
		if(getGridEXTable().getUseColumnSets() && getGridEXTable().getPreviewRow())
		{
			var _previewTables = null;
			if(document.getChildsById != null)
				_previewTables = document.getChildsById("preview" + getGridEXTable().getID()); 
			else
				_previewTables = allChildsCore("preview" + getGridEXTable().getID(),document.getElementsByTagName("*"));
			var _tableWidth = getGridEXTable().getWidth(); 
			for(var _item = 0; _item < _previewTables.length; _item++)
			{
				var _previewCol = _previewTables[_item].getElementsByTagName("COL")[0];
				if(_previewCol.getAttribute("offset") != null)					
					_previewCol.width = (_tableWidth - _previewCol.getAttribute("offset")) + "px";
				else	
					_previewCol.width = _tableWidth + "px"; 
			}
		}
		if(getGridEX().getFixTableSize())
			fixTableSize(getGridEX().getRootTable()); 	
	}	
	function CopyCellsWidth(cells)
	{		
		var l = -1;
		if(htmlTable.cells != null)
			l = htmlTable.cells.length; 
		else
			l = getCellsCore(htmlTable).length;
		var cell = null; 
		for(var i = 0; i < l; i++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[i]; 
			else
				cell = getCellsCore(htmlTable)[i]; 				
			cells[cells.length] = cell.offsetWidth;
		}
	}
	function columnset_onselectstart() { }	
	function column_onmousedown()
	{		
		var column = getColumnFromElement(window.event.srcElement);	
		if(column.getAttribute("type") == "space")
			return; 
									
		if((column.id == null || column.id.length == 0) && (column.getAttribute("type") != "header"))
			return; 
			
		if(window.event.srcElement != null && window.event.srcElement.tagName == "INPUT")
			return; 
		
		if(!columnResizing)
		{										
			if(column.getAttribute("type") == "header")
			{
				if(column.style.cursor == cursorResize)
					startColumnSetResize(column, gridEXColumnSet); 
			}
			else
			{	
				var gridEXColumn = getGridEXTable().getColumns().getGridEXColumnByClientID(column.id);
				if(gridEXColumn.getAllowSize() || gridEXColumn.getAllowDrag()) 
				{					
					if(column.style.cursor == cursorResize && gridEXColumn.getAllowSize())
					{
						couldStartResize = true; 
						resizepoint = new Point(window.event.clientX, window.event.clientY);
						couldResizeColumn = column; 
						couldResizeColumnSet = gridEXColumnSet; 						
					}
					else if(gridEXColumn.getAllowDrag())
					{					
						if(column.getAttribute("type") != null && column.getAttribute("type") == "header")
						{
							window.event.cancelBubble = true; 
							return; 
						}
						if(gridEXColumn.getActAsSelector())
							return;								
						if(getGridEX().getThemedAreas() == 1)
							ShowColumnPressed(column, true);
						if(gridEXColumn.getAllowDrag())
						{
							couldStartDrag = true; 
							couldDragColumn = column;
							couldDragHeader = getGridEXHeader(); 
							dragpoint = new Point(window.event.clientX, window.event.clientY); 
						}
					}
				}
			}		
		}
		window.event.cancelBubble = true; 
		window.event.returnValue = false;   
	}	
	function column_onmousemove()
	{		
		if(couldStartDrag && dragpoint != null)
		{
			if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
			{
				var column = getColumnFromElement(window.event.srcElement); 
				startColumnSetDrag(column, getGridEXHeader(), gridEXColumnSet);
				couldStartDrag = false; 
				couldDragColumn = couldDragHeader = null;
				dragpoint = null; 
			}
		}
		else if(couldStartResize && resizepoint != null)
		{
			if(Math.abs(window.event.clientX - resizepoint.X()) > 1 || Math.abs(window.event.clientY - resizepoint.Y()) > 1)
			{
				startColumnSetResize(couldResizeColumn, couldResizeColumnSet);				
				couldStartResize = false; 				
				couldResizeColumn = null; 
				couldResizeCoumnSet = null;
				resizepoint = null; 
			}			
		}
		else if(!columnResizing)
		{	
			var column = getColumnFromElement(window.event.srcElement);
			if(column.getAttribute("allowsize") == null) 
			{
				if(isInResizeArea(column))
					column.style.cursor = cursorResize;
				else
					column.style.cursor = "default"; 
			}
		}
	}	
	function column_onmouseout() {	}	
	function column_onmouseover()	
	{		
		var column = getColumnFromElement(window.event.srcElement);
		if(column.getAttribute("allowsize") == null) 
		{
			if(columnResizing)
				column.style.cursor = cursorResize;
			else
			{				
				if(isInResizeArea(column))
					column.style.cursor = cursorResize;
				else
					column.style.cursor = "default";
			}
		}		
	}
	var eventButton = 0; 
	function column_onmouseup()
	{
		eventButton = window.event.button; 
	}
	function column_onclick()
	{		
		var tdColumn = getColumnFromElement(window.event.srcElement); 		
		if(!columnResizing && !columnDraging && !canceledByUser && tdColumn.style.cursor != cursorResize)
		{							
			if(tdColumn.getAttribute("type") != null && (tdColumn.getAttribute("type") == "header" || tdColumn.getAttribute("type") == "space"))
				return; 
			
			var column = getGridEXTable().getColumns().getGridEXColumnByClientID(tdColumn.getAttribute("id")); 
			if(column.getAllowSort())
			{		
				if(column.getActAsSelector())
					return;
					
				var cancel = getGridEX().FireEvent("ColumnHeaderClick", [column]);
				if(cancel == null || !cancel)
				{					
					var input = document.getElementsByName(getGridEX().getID() + "_eventdata")[0]; 
					if(input == null)
						throw Error("sort info field is null");
				
					input.value = getGridEXTable().getID() + ":" + column.getClientID();
					getGridEX().DoPostBack(null, "ColumnHeaderClick"); 
				}
			}
		}
		else if(columnDraging)
			drag_onmouseup(); 
			
		if(gridEXTable.getGridEX().getThemedAreas() == 1)
		{
			ShowColumnUnPressed();
			currpressedcolumn = null;
			couldStartDrag = false;
			couldDragColumn = couldDragHeader = null;
			dragpoint = null; 
		}			
		if(tdColumn.style.cursor != cursorResize)
		{
			window.event.cancelBubble = true; 
			window.event.returnValue = false;
		}
		getGridEXTable().getGridEX().setHitTestArea(6); 
		getGridEXTable().getGridEX().FireEvent("Click", [getGridEXTable().getGridEX(),window.event.type == "contextmenu" ? 2 : 1, window.event.clientX, window.event.clientY]);
	}
	function column_oncontextmenu()
	{
		window.event.cancelBubble = true; 
		window.event.returnValue = false; 
	}
	function column_ondblclick()
	{				
		cancelColumnSetResize();
		var column = getColumnFromElement(window.event.srcElement); 
		if(column.getAttribute("type") == "header") 
			return; 
			
		if(column.style.cursor == cursorResize)
		{			
			column.style.cursor = "default"; 			
			var columnsetswidth = getGridEXHeader().getColumnSets().getColumnSetsCoreWidth();
			var oldColumnSetWidth = htmlTable.offsetWidth; 		
			var oldColumnSize = column.offsetWidth; 
			var gridEXColumn = getGridEXTable().getColumns().getGridEXColumnByClientID(column.id);
			var maxColumnSize = getMaximumColumnSize(gridEXColumn);
			if(maxColumnSize <= 0)
				return; 
				
			if(column.getAttribute("type") == "ch") 
				maxColumnSize += getGridEXTable().getHeaderWidth();
				
			var offset = (maxColumnSize - oldColumnSize); 
			var newColumnSetWidth = oldColumnSetWidth + offset; 		
			if(column.colSpan == getColumnCount())
			{
				AutoSizeColumns(maxColumnSize, oldColumnSetWidth); 
				if(getGridEX().getColumnAutoResize())
					getGridEXHeader().getColumnSets().AutoSizeByColumnSet(getIndex(), columnsetswidth - oldColumnSetWidth, Math.abs(columnsetswidth - htmlTable.parentElement.offsetWidth), columnsetswidth); 
				else
					AutoSizeItems(); 
			}
			else
			{
				ResizeCellsInSet(column, maxColumnSize, oldColumnSize, oldColumnSetWidth - oldColumnSize, newColumnSetWidth - maxColumnSize); 
				if(getGridEX().getColumnAutoResize())
					getGridEXHeader().getColumnSets().AutoSizeByColumnSet(getIndex(), columnsetswidth - oldColumnSetWidth, columnsetswidth - htmlTable.parentElement.offsetWidth, columnsetswidth); 								
				else
					AutoSizeItems(); 
			}
		}
		getGridEXTable().getGridEX().setHitTestArea(6); 
		getGridEXTable().getGridEX().FireEvent("DoubleClick", [getGridEXTable().getGridEX(), eventButton, window.event.clientX, window.event.clientY]); 
	}
	function column_onselectstart() { }		
	function getFixedWidth()
	{		
		var fixedwidth = 0;
		var l = -1;
		if(htmlTable.cells != null)
			l = htmlTable.cells.length;
		else
			l = getCellsCore(htmlTable).length; 
		for(var i = 0; i < l; i++)
		{
			var cell = null;
			if(htmlTable.cells != null)
				cell = htmlTable.cells[i];
			else
				cell = getCellsCore(htmlTable)[i];
			if(cell.allowsize != null)
				fixedwidth += cell.offsetWidth; 
		}
		return fixedwidth;
	}	
	function isMostRight(td)
	{		
		if(td.getAttribute("mrc") != null)
			return true;
		else if(td.offsetLeft + td.offsetWidth >= htmlTable.offsetWidth) 
			return true;
		else
			return false; 
	}	
	function isInResizeArea(td)
	{
		var x = window.event.offsetX;
		var y = window.event.offsetY;		
		var ylow = 0;
		var yhigh = ylow + td.offsetHeight; 		
		var xlow;
		var xhigh; 				
		if(isMostRight(td))
		{
			xlow = td.offsetWidth - 4;
			xhigh = td.offsetWidth + 4; 		
		}
		else
		{
			xlow = td.offsetWidth - 5;
			xhigh = td.offsetWidth; 
		}		
		if((x >= xlow && x <= xhigh) && (y >= ylow && y <= yhigh))		
			return true;
		else
			return false;
	}		
	function ResizeCellsInSet(column, cellsize, oldcellsize, oldsize, newsize)
	{			
		var cellwidth = null; 
		var diff = null; 		
		var cell = null; 
		var _cellsLength = -1;
		if(htmlTable.cells != null)
			_cellsLength = htmlTable.cells.length;
		else
			_cellsLength = getCellsCore(htmlTable).length;
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var _inallHeaders = getGridEXTable().getHeaders().length > 1;
		var _fixedwidth = null;
		var col = parseInt(column.getAttribute("usecol"), 10);
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell.getAttribute("allowsize") != null && cell.getAttribute("usecol") != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
				_fixedwidth[cell.getAttribute("usecol")] = cell.offsetWidth; 
			}
		}
		var colswidth = new Array(_cols.length); 		
		for(var icol = 0; icol < _cols.length; icol++)
			colswidth[icol] = getPixelColWidth(_cols[icol].width); 
		if(_fixedwidth != null)
		{
			for(var i = 0; i < _fixedwidth.length; i++)
			{
				if(_fixedwidth[i] != null)
				{
					newsize -= _fixedwidth[i];
					oldsize -= _fixedwidth[i];
				}
			}
		}
		if(newsize <= 0 || oldsize <= 0)
			return; 
			
		var _colwidth = 0; 
		var fixedcols = new Array(_cols.length); 
		var fixedwidth = 0; 
		var colspan = column.colSpan + parseInt(column.getAttribute("usecol"), 10); 
		for(var icol = parseInt(column.getAttribute("usecol"), 10); icol < colspan; icol++)
		{
			_colwidth = Math.round((colswidth[icol] * cellsize) /  oldcellsize);
			if(_colwidth > 0)
				_cols[icol].width = _colwidth + "px";
			else
				_cols[icol].width = "1px"; 
			fixedcols[icol] = 1; 
			fixedwidth += getPixelColWidth(_cols[icol].width); 
		}
		diff = getPaddingLeft(column) + getPaddingRight(column) + getBorderWidth(column) + getSortWidth(column);
		column.style.width = fixedwidth + "px"; 
		column.childNodes[0].style.width = fixedwidth + "px"; 
		if(fixedwidth - diff > 0)
			getInnerSpan(column.childNodes[0]).style.width = (fixedwidth - diff) + "px"; 
		else
			getInnerSpan(column.childNodes[0]).style.width = "0px"; 
		for(var icell = 0; icell < _cellsLength; icell++)
		{	
			if(htmlTable.cells != null)
				cell = htmlTable.cells[icell];
			else
				cell = getCellsCore(htmlTable)[icell];
			if(cell != column)
			{
				if(cell.getAttribute("allowsize") == null)
				{
					if(cell.getAttribute("type") == "header" || cell.colSpan == getColumnCount())
					{									
						if(cell.getAttribute("type") == "header")
						{
							cell.style.width = cellsize + "px"; 
							cell.childNodes[0].style.width = cellsize + "px"; 
						}
						else if(cellCouldResizeOthers(icell, _cellsLength))
						{						
							fixedwidth = 0; 
							colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10); 
							for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
							{
								if(fixedcols[icol] == null)
								{
									_cols[icol].width = Math.round((colswidth[icol] * newsize) / oldsize) + "px"; 							
									fixedcols[icol] = 1;
								}
								fixedwidth += getPixelColWidth(_cols[icol].width); 
							}
							diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);
							cell.style.width = fixedwidth + "px"; 
							cell.childNodes[0].style.width = fixedwidth + "px";
							if(fixedwidth - diff > 0)
								getInnerSpan(cell.childNodes[0]).style.width = (fixedwidth - diff) + "px";
							else
								getInnerSpan(cell.childNodes[0]).style.width = "0px"; 
						}
						if(_inallHeaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 
					}			
					else if(cell.getAttribute("usecol") != null && cell.getAttribute("usecol") == col && column.colSpan == cell.colSpan)
					{									
						fixedwidth = 0; 
						colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10); 
						for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
						{
							if(fixedcols[icol] == null)
							{							
								_cols[icol].width = Math.round((colswidth[icol] * cellsize) / oldcellsize) + "px"; 							
								fixedcols[icol] = 1;
							}
							fixedwidth += getPixelColWidth(_cols[icol].width);
						}
						diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
						cell.style.width = fixedwidth + "px"; 
						cell.childNodes[0].style.width = fixedwidth + "px"; 
						if(fixedwidth - diff > 0)
							getInnerSpan(cell.childNodes[0]).style.width = (fixedwidth - diff) + "px";
						else
							getInnerSpan(cell.childNodes[0]).style.width = "0px"; 
						if(_inallHeaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 
					}
					else	 if(cell.getAttribute("type") != "space")
					{	
						fixedwidth = 0; 
						colspan = cell.colSpan + parseInt(cell.getAttribute("usecol"), 10); 
						for(var icol = parseInt(cell.getAttribute("usecol"), 10); icol < colspan; icol++)
						{
							if(fixedcols[icol] == null)
							{
								_cols[icol].width = Math.round((colswidth[icol] * newsize) / oldsize) + "px";
								fixedcols[icol] = 1;
							}
							fixedwidth += getPixelColWidth(_cols[icol].width);
						}										
						diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
						cell.style.width = fixedwidth + "px"; 
						cell.childNodes[0].style.width = fixedwidth + "px"; 
						if(fixedwidth - diff > 0)
							getInnerSpan(cell.childNodes[0]).style.width = (fixedwidth - diff) + "px";
						else
							getInnerSpan(cell.childNodes[0]).style.width = "0px"; 
						if(_inallHeaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols);
					}			
				}
			}
		}		
	}		
	var cellsch = null; 
	var fixedwidth = getFixedWidth();	
	if(getIsInHeader())
	{
		var cell = null;		
		htmlTable.parentElement.addEventListener("selectstart", columnset_onselectstart, false); 
		htmlTable.addEventListener("selectstart", columnset_onselectstart, false);		
		for(var irow = 0; irow < htmlTable.rows.length; irow++)
		{
			for(var icell = 0; icell < htmlTable.rows[irow].cells.length; icell++)
			{
				cell = htmlTable.rows[irow].cells[icell];
				if(cell.getAttribute("type") != "space")
				{
					cell.addEventListener("mousemove", column_onmousemove, false); 
					cell.addEventListener("mouseover", column_onmouseover, false);
					cell.addEventListener("mouseout", column_onmouseout, false);
					cell.addEventListener("mousedown", column_onmousedown, false);  
					cell.addEventListener("mouseup", column_onmouseup, false); 
					cell.addEventListener("click", column_onclick, false); 
					cell.addEventListener("dblclick", column_ondblclick, false); 
					cell.addEventListener("selectstart", column_onselectstart, false); 
					cell.addEventListener("contextmenu", column_oncontextmenu, false); 
				}
				if(cell.getAttribute("type") == "ch")
				{
					if(cellsch == null)
						cellsch = new Array(); 
						
					cellsch[cellsch.length] = cell; 					
				}				
			}
		}
	}	
	var gridEXColumnSet = this;	
	return this;	
}
function GridEXColumn(definition, index, table)
{
	var clientID = null; 	
	var actAsSelector = false; 
	var allowDrag = false; 
	var allowGroup = false; 
	var allowSize = false; 
	var allowSort = false; 	
	this.characterCasing = 2; 
	var cssCellsResize = null; 
	var dropdownID = ""; 	
	var columnIndex = -1;
	var columnType = -1; 
	var editTarget = 2;
	var editType = -1; 
	var filterEditType = -1; 
	var filterListID = ""; 
	var keepColumnExpand = false; 	
	var key = null; 
	var inputMask = null;
	var invalidValueAction = -1; 
	var invalidValueMessage = ""; 
	var maxlength = 0;
	var passwordChar = ""; 
	var scrollbars = -1; 
	var selectable = true;
	var visible = true;  
	var multiLineEdit = false; 
	var position = -1;
	var table = table; 
	clientID = definition[0]; 
	key = definition[1]; 
	actAsSelector = (definition[2] == 1) ? true : false;
	allowDrag = (definition[3] == 1) ? true : false; 
	allowGroup = (definition[4] == 1) ?  true : false;
	allowSize = (definition[5] == 1) ? true : false; 
	allowSort = (definition[6] == 1) ? true : false;
	this.position = definition[7];
	selectable = (definition[8] == 1) ? true : false; 
	visible = (definition[9] == 1) ? true : false; 
	columnSetColumn = definition[10];
	keepColumnExpand = (definition[11] == 1) ? true : false; 	
	cssCellsResize = definition[12];
	columnType = definition[13];
	editType = definition[14]; 
	inputMask = definition[15];
	editTarget = definition[16];
	maxlength = definition[17]; 
	scrollbars = definition[18]; 
	multiLineEdit = (definition[19] == 1) ? true : false;
	this.characterCasing = definition[20]; 
	invalidValueAction = definition[21]; 
	invalidValueMessage = definition[22]; 
	passwordChar = definition[23];
	dropdownID = definition[24];
	filterEditType = definition[25]; 
	filterListID = definition[26]; 	
	this.isGrouped = (definition[27] == 1) ? true : false; 
	this.limitToList = (definition[28] == 1) ? true : false; 
	this.datePattern = definition[29];
	if(index != null)
		columnIndex = index; 
	this.getActAsSelector = getActAsSelector; 
	this.getAllowDrag = getAllowDrag; 
	this.getAllowGroup = getAllowGroup; 
	this.getAllowSize = getAllowSize; 
	this.getAllowSort = getAllowSort; 
	this.getColumnSetColumn = getColumnSetColumn; 
	this.getColumnType = getColumnType;
	this.getColumnIndex = getColumnIndex; 
	this.getClientID = getClientID; 
	this.getDropDownID = getDropDownID; 
	this.getEditTarget = getEditTarget; 
	this.getEditType = getEditType; 
	this.getFilterEditType = getFilterEditType; 
	this.getFilterListID = getFilterListID; 
	this.getInputMask = getInputMask; 
	this.getInvalidValueAction = getInvalidValueAction; 
	this.getInvalidValueMessage = getInvalidValueMessage; 
	this.getKeepColumnExpand= getKeepColumnExpand;
	this.getKey = getKey; 
	this.getMaxLength = getMaxLength;
	this.getMultiLineEdit = getMultiLineEdit;
	this.getPasswordChar = getPasswordChar; 
	this.getPosition = getPosition; 
	this.getScrollBars = getScrollBars; 
	this.getSelectable = getSelectable; 
	this.getTable = getTable;
	this.getVisible = getVisible;
	this.AutoSize = AutoSize;
	function getClientID() { return clientID; }
	function getActAsSelector() { return actAsSelector; }
	function getAllowDrag() { return allowDrag; }	
	function getAllowGroup() { return allowGroup; }	
	function getAllowSize() { return allowSize; }	
	function getAllowSort() { return allowSort; }	
	function getColumnIndex() { return columnIndex; }	
	function getDropDownID() { return dropdownID; }	
	function getColumnSetColumn() { return columnSetColumn; }
	function getColumnType() { return columnType; }	
	function getEditTarget() { return editTarget; }	
	function getEditType() { return editType; }	
	function getFilterEditType() { return filterEditType; }	
	function getFilterListID() { return filterListID; }	
	function getInputMask() { return inputMask; }	
	function getInvalidValueAction() { return invalidValueAction; }	
	function getInvalidValueMessage() { return invalidValueMessage; }	
	function getKeepColumnExpand() { return keepColumnExpand; }	
	function getKey() { return key; }	
	function getMaxLength() {	return maxlength; }	
	function getMultiLineEdit() { return multiLineEdit; }
	function getPasswordChar() { return passwordChar; }
	function getPosition() { return this.position; }
	function getScrollBars() { return scrollbars; }	
	function getSelectable() { return selectable; }
	function getTable() { return table; }
	function getVisible() { return visible; }		
	function AutoSize()
	{
		if(!getVisible() || !getAllowSize())
			return; 
			
		table.AutoSizeColumn(gcol); 
	}
	var gcol = this; 
	return this;
}
function GridEXColumnCollection(array, table)
{	
	if(array == null)
		throw Error("columns array is null"); 
		
	var table = table; 
	var columnDefinitionLength = 30; 		
	if(array.length % columnDefinitionLength != 0)
		throw Error("columns array length is invalid");
	
	var column = null; 	
	var columnArray = null; 
	var columnIndex = 0; 
	var columns = new Array();
	for(var index = 0; index < array.length; index = index + columnDefinitionLength)
	{		
		columnArray = new Array();
		for(var prop = index; prop < index + columnDefinitionLength; prop++)
			columnArray[columnArray.length] = array[prop];
			
		column = new GridEXColumn(columnArray, columnIndex, table); 
		columns[columns.length] = column; 
		columnIndex++; 
	}		
	var hiddenColumnsCount = null;	
	this.getGridEXColumnByClientID = getGridEXColumnByClientID; 
	this.getGridEXColumn = getGridEXColumn;
	this.Count = Count;
	this.HiddenColumnsCount = HiddenColumnsCount;	
	function Count()
	{
		return columns.length; 
	}	
	function HiddenColumnsCount()
	{		
		if(hiddenColumnsCount == null)
		{			
			hiddenColumnsCount = 0; 			
			for(var icolumn = 0; icolumn < Count(); icolumn++)
			{				
				if(!columns[icolumn].getVisible())
					hiddenColumnsCount++;
			}		
		}		
		return hiddenColumnsCount; 
	}	
	function getGridEXColumn(index)
	{
		if(index < 0 || index >= Count())
			throw Error("index out of range"); 
			
		return columns[index]; 
	}	
	function getGridEXColumnByClientID(id)
	{				
		var column = null; 
		for(var index = 0; index < columns.length; index++)
		{
			column = columns[index]; 
			if(column.getClientID() == id)
				return column; 
		}		
		throw Error("argument out of range"); 
	}
}
function GridEXCell(column, row)
{
	var column = column; 
	var current = false; 	
	var cellValue = null;	
	var editManager = null; 
	var filterManager = null; 
	var row = row; 	
	var valueType = -1;
	var innerCell = null; 		
	this.dataChanged = false;
	if(column.getVisible())
	{	
		innerCell = null; 
		if(row.getInnerRow().getChildsById != null)
			innerCell = row.getInnerRow().getChildsById(column.getClientID() + "_L")[0]; 		
		else
			innerCell = allChildsCore(column.getClientID() + "_L", row.getInnerRow().getElementsByTagName("*"))[0];
		if(innerCell == null)
			throw Error("unable to find cell object");		
			
		valueType = 1;
		if(column.getEditType() == 9 || column.getFilterEditType() == 19)
		{
			if(getInnerSpan(innerCell.childNodes[0]).childNodes.length == 1)
			{
				var element = getInnerSpan(innerCell.childNodes[0]).getElementsByTagName("INPUT")[0];
				if(element.nodeType == 1 && element.tagName == "INPUT" && element.getAttribute("type") == "checkbox")
				{
					cellValue = element.checked;
					try
					{
						if(window.event != null && window.event.srcElement != null && window.event.type == "click" && window.event.srcElement == element)
							cellValue = !cellValue; 
					} 
					catch(err) { }
				}
			}
		}		
	}
	else if(row.getGridEX().getUseHiddenColumns())
	{
		var hiddenValues = row.getGridEX().FindHiddenValuesByRow(row);		
		if(hiddenValues != null)
		{			
			var l = hiddenValues.length;
			for(var i = 0; i < l; i = i + 2)
			{
				if(hiddenValues[i] == column.getColumnIndex())
				{
					cellValue = normalizeValue(hiddenValues[i+1]); 
					i = l; 
				}
			}
		}		
		valueType = 2;
	}
	var cssName = ""; 
	this.cssName = cssName; 
	this.getColumn = getColumn;
	this.getCurrent = getCurrent; 
	this.getDataChanged = getDataChanged;
	this.getEditManager = getEditManager;
	this.getFilterManager = getFilterManager; 
	this.getGridEX = getGridEX; 
	this.getInnerCell = getInnerCell;	
	this.getRow = getRow; 
	this.getText = getText; 
	this.setText = setText;
	this.getValue = getValue;
	this.isNewRecordTopCell = isNewRecordTopCell; 
	this.setChecked = setChecked; 	
	this.setValue = setValue;	
	this.HideFilterCell = HideFilterCell; 
	this.ResumeEdit = ResumeEdit; 
	this.ResumeFilter = ResumeFilter; 
	this.ShowEditorCell = ShowEditorCell; 
	this.ShowFilterCell = ShowFilterCell; 
	this.UndoChanges = UndoChanges; 	
	function ResumeEdit()
	{		
		if(editManager == null)
			throw Error("invalid operation exception: invalid edit manager"); 
			
		getGridEX().ResumeEditOperation(); 
	}	
	function ResumeFilter()
	{
		if(getRow().getType() != 11)
			throw Error("invalid operation exception: invalid filter row");
			
		if(filterManager == null)
			throw Error("invalid operation exception: invalid filter manager"); 
			
		getGridEX().ResumeFilterOperation(); 
	}	
	function HideFilterCell()
	{
		if(filterManager != null && filterManager.Hide != null)
			filterManager.Hide(); 
	}	
	function ShowFilterCell()
	{	
		if(getColumn().getSelectable())
		{
			if(getColumn().getFilterEditType() == -1)
				return;
			if(getColumn().getFilterEditType() == 19)
			{
				if(window.event.srcElement == innerCell.getElementsByTagName("INPUT")[0])
				{
					if(cellValue != innerCell.getElementsByTagName("INPUT")[0].checked)
					{
						this.dataChanged = true; 
						cellValue = innerCell.getElementsByTagName("INPUT")[0].checked; 
					}
				}
			}			
			getFilterManager().Show(); 							
		}
	}	
	function ShowEditorCell()
	{
		if(getColumn().getSelectable())
		{
			if(getColumn().getEditType() != -1)
			{								
				if(getColumn().getEditType() == 9)	
				{
					if(getRow().getRowType() == "Record" && getGridEX().getUpdateMode() == 2 && getGridEX().getUpdateOnLeave())
						getGridEX().UpdateData(false);
						
					return;
				}
				if(getTable().getHierarchicalMode() == 2)
				{
					if(window.event != null && window.event.type == "click")
					{
						if(window.event.srcElement.tagName == "SPAN" && window.event.srcElement.onclick != null)
							return;
						else if(window.event.srcElement.tagName == "SPAN") 
						{							
							var tmpParent = window.event.srcElement.parentElement; 
							if(tmpParent != null && tmpParent.tagName == "SPAN" && tmpParent.onclick != null)
								return; 
						}
					}
				}
				getEditManager().Show(); 								
			}
		}
	}	
	function UndoChanges()
	{
		this.dataChanged = false; 
		getRow().ShowHeaderIndicator(false); 
	}	
	function getColumn()
	{
		return column; 
	}	
	function getCurrent()
	{
		return current; 
	}	
	function getDataChanged()
	{
		if(getColumn().getColumnType() == 4 && getColumn().getEditType() == 9)		
			return (cellValue != getValue());
		else	
			return this.dataChanged; 
	}	
	function getFilterManager()
	{
		if(getRow().getType() != 11)
			return null;
			
		if(filterManager == null)
		{			
			switch(column.getFilterEditType())
			{			
				case 2:
					if(column.getPasswordChar().length > 0)	
						filterManager = new GridEXFilterPasswordManager(gridEXCell, false); 
					else
						filterManager = new GridEXFilterTextBoxManager(gridEXCell, false); 
				break; 
				case 4:
					filterManager = new GridEXFilterComboManager(gridEXCell, false); 
				break; 				
				case 5:
					filterManager = new GridEXFilterListManager(gridEXCell); 
				break;				
				case 12:
					if(column.getPasswordChar().length > 0)	
						filterManager = new GridEXFilterPasswordManager(gridEXCell, true); 
					else
						filterManager = new GridEXFilterTextBoxManager(gridEXCell, true); 
				break; 
				case 13:
					filterManager = new GridEXFilterCalendarDropDownManager(gridEXCell);
				break;
				case 14:
					filterManager = new GridEXFilterCalendarComboManager(gridEXCell); 
				break; 
				case 15:
					filterManager = new GridEXFilterComboManager(gridEXCell, true); 
				break; 
				case 16:
					filterManager = new GridEXFilterListManager(gridEXCell, true); 
				break;				
				case 18:
					filterManager = new GridEXFilterDropDownManager(gridEXCell); 
				break; 
				case 19:
					filterManager = new GridEXFilterCheckBoxManager(gridEXCell); 
				break;
				case 17:
					filterManager = new GridEXFilterComboDropDownManager(gridEXCell); 
				break;
			}
		}
		return filterManager; 
	}	
	function getEditManager()
	{	
		if(editManager == null) 
		{
			switch(column.getEditType())
			{
				case 2:
					if(column.getMultiLineEdit())
						editManager = new GridEXEditTextAreaManager(gridEXCell); 
					else
					{
						if(column.getPasswordChar().length > 0)
							editManager = new GridEXEditPasswordManager(gridEXCell); 
						else
							editManager = new GridEXEditTextBoxManager(gridEXCell);	
					}
				break;				
				case 3:
					editManager = new GridEXCalendarDropDownManager(gridEXCell);
				break;				
				case 4:
					editManager = new GridEXCalendarComboDropDownManager(gridEXCell); 				
				break; 				
				case 5:
					editManager = new GridEXComboListManager(gridEXCell); 
				break; 				
				case 6:
					editManager = new GridEXEditValueListManager(gridEXCell);
				break;				
				case 7:
					editManager = new GridEXComboDropDownManager(gridEXCell); 
				break; 				
				case 8:
					editManager = new GridEXDropDownManager(gridEXCell); 
				break; 				
				case 9:
					//editManager = new GridEXCheckBoxManager(gridEXCell); 
				break; 				
				default:				
					throw Error("unable to retrieve a edit Manager"); 
				break; 
			}
		}
		return editManager;
	}	
	function getGridEX()
	{
		return row.getGridEX(); 
	}	
	function getInnerCell()
	{
		return innerCell; 
	}	
	function getRow()
	{
		return row; 
	}
	function getTable()
	{
		return getRow().getTable(); 
	}
	function getText()
	{
		if(innerCell == null)
			return ""; 
		if(innerCell.childNodes.length == 0)
			return "";
		if(column.getEditType() == 9 || column.getFilterEditType() == 19)		
			return String(getValue());
		if(column.getPasswordChar().length > 0 && innerCell.getAttribute("text") != null)
			return innerCell.getAttribute("text");
		return trim(getInnerSpan(innerCell.childNodes[0]).innerText); 		
	}
	function setText(text)
	{
		if(innerCell == null || innerCell.childNodes.length == 0)
			return;
		if(column.getEditType() == 9 || column.getFilterEditType() == 19)
			return;
		if(column.getPasswordChar().length > 0 && innerCell.getAttribute("text") != null)
			innerCell.setAttribute("text", text);
		else
			getInnerSpan(innerCell.childNodes[0]).innerText = text; 			
	}
	function getValue()
	{	
		if(valueType == 1)
		{	
			if(getColumn().getColumnType() == 4 || getColumn().getActAsSelector()) 
			{
				var _innerSpan = getInnerSpan(innerCell.childNodes[0]); 
				if(_innerSpan.childNodes.length > 0)
				{					
					var element = _innerSpan.getElementsByTagName("INPUT"); 
					for(var i = 0; i < element.length; i++)
					{
						if(element[i].tagName == "INPUT" && element[i].getAttribute("type") == "checkbox")
							return element[i].checked; 
					}
				}
			}
			else
			{	
				if(innerCell.getAttribute("value") != null)
					return innerCell.getAttribute("value"); 
				else
					return getText();
			}
		}
		else if(valueType == 2)
			return cellValue;
		else
			return null; 		
	}	
	function setValue(value)
	{		
		if(valueType == 1)
		{
			if(getColumn().getColumnType() == 4 || getColumn().getActAsSelector())
			{
				if(innerCell.childNodes[0].getElementsByTagName("SPAN")[0].childNodes.length > 0)
				{
					var element = innerCell.childNodes[0].getElementsByTagName("SPAN")[0].getElementsByTagName("INPUT");
					if(element.length > 0)
					{
						for(var ielement = 0; ielement < element.length; ielement++)
						{
							if(element[ielement].getAttribute("type") == "checkbox")
							{
								element[ielement].checked = value; 
								ielement = element.length; 
							}
						}
					}
					else if(element.getAttribute("type") == "checkbox")
						element.checked = value; 
					else
						innerCell.setAttribute("value", value);
				}
				else
					innerCell.setAttribute("value", value);
			}
			else			
				innerCell.setAttribute("value", value);
		}
		else if(valueType == 2)
			cellValue = value; 
			
		if(valueType == 1 || valueType == 2)
		{
			this.dataChanged = true; 
			if((getRow().getRowType() == "Record" && getGridEX().getUpdateMode() == 2 && getGridEX().getUpdateOnLeave()) || getRow().getRowType() == "NewRecord")
			{
				if(getRow().getRowType() == "NewRecord")
					getGridEX().UpdateData(false, false);
				else
					getGridEX().UpdateData(false); 
			}
		}
	}	
	function isNewRecordTopCell()
	{
		return (getRow().getType() == 9 && getRow().getTable().getParent() == null && getRow().getTable().getNewRowPosition() == 2); 
	}	
	function setChecked()
	{
		this.dataChanged = true; 
	}	
	var gridEXCell = this; 	
	return this; 
}
var currentRowHeader = null;
function GridEXRow(id, innerRow, table, pos, rootRow)
{	
	var cells = null; 
	var childRows = -1; 
	var currentCell = null; 	
	var columnSetsCount = 0; 
	var currentColumnSet = null; 
	var currentColumnSetRow = null; 
	var dataKeyValues = null; 
	var headerIndicatorType	= -1; 
	var id = id;
	var ischecked = false;	
	var expanded = false; 	
	var innerRow = innerRow; 	
	var isAlternating = false; 
	var parentRow = -1; 
	var position = -1; 
	var previewInnerRow = null; 
	var rootInnerRow = null; 
	if(rootRow != null)
		rootInnerRow = rootRow; 	
	var rowHeaderCell = null; 	
	var table = table; 
	var type = null;
	if(innerRow == null)
		throw Error("innerRow for GridEXRow is null or invalid"); 
	
	if(innerRow.getAttribute("type") != null)
		type = parseInt(innerRow.getAttribute("type"), 10);
	else
		type = 3;
		
	if(innerRow.getAttribute("alt") == null || innerRow.getAttribute("alt").length == 0)
		isAlternating = false;
	else
		isAlternating = true; 
		
	if(pos != null)
		position = pos; 
	
	this.current = false;
	this.containsURL = containsURL; 
	this.getCellByColumnID = getCellByColumnID; 
	this.getCellByColumnKey = getCellByColumnKey;
	this.getCellByIndex = getCellByIndex; 	
	this.getCellSelected = getCellSelected; 
	this.getCellsLength = getCellsLength; 
	this.getChildRows = getChildRows;
	this.getCurrentCell = getCurrentCell; 
	this.getDataKeyValues = getDataKeyValues; 
	this.getDataChanged = getDataChanged; 	
	this.getExpanded = getExpanded; 
	this.getGridEX = getGridEX; 
	this.getID = getID; 
	this.getInnerRow = getInnerRow; 
	this.getIsAlternating = getIsAlternating; 
	this.getIsChecked = getIsChecked; 
	this.getIsVisible = getIsVisible; 
	this.getNextRow = getNextRow; 	
	this.getParent = getParent; 
	this.getPosition = getPosition; 
	this.getPreviousRow = getPreviousRow;
	this.getPreviewInnerRow = getPreviewInnerRow; 
	this.getRowHeight = getRowHeight; 
	this.getRowType = getRowType; 
	this.getSelected = getSelected; 
	this.getTable = getTable;
	this.getType = getType; 
	this.getURL = getURL; 
	this.getURLTarget = getURLTarget;
	this.getVisibleInScroll = getVisibleInScroll;	
	this.toogleIndicator = toogleIndicator;
	this.setCurrentCell = setCurrentCell;	
	this.BeforeEdit = BeforeEdit;
	this.BeforeFilter = BeforeFilter;
	this.CheckRow = CheckRow; 
	this.Collapsing = Collapsing; 
	this.CollapsePreviewRow = CollapsePreviewRow; 	
	this.Expanding = Expanding; 
	this.HideHeaderIndicator = HideHeaderIndicator; 
	this.NextFocusCell = NextFocusCell; 
	this.PreviousFocusCell = PreviousFocusCell; 
	this.ReportStatus = ReportStatus; 
	this.ShowHeaderIndicator = ShowHeaderIndicator; 
	this.TabChanged = TabChanged; 
	this.TabChanging = TabChanging; 	
	function getGridEX()
	{
		return table.getGridEX(); 
	}	
	function getID()
	{
		return id; 
	}	
	function getTable()
	{
		return table; 
	}	
	function getCellByColumnID(columnID)
	{
		RetrieveCells();
		var cell = null; 
		var l = cells.length; 
		for(var i = 0; i < l; i++)
		{
			cell = cells[i]; 			
			if(cell.getColumn().getClientID() == columnID)
				return cell; 
		}
		throw Error("argument out of range");
	}	
	function getCellByColumnKey(columnKey)
	{
		RetrieveCells();
		var cell = null;
		var l = cells.length; 
		for(var i = 0; i < l; i++)
		{			
			cell = cells[i]; 
			if(cell.getColumn().getKey() == columnKey)
				return cell; 
		}
		throw Error("argument out of range"); 
	}	
	function getCellByIndex(index)
	{
		RetrieveCells();
		if(index < 0 || index >= cells.length)
			throw Error("argument out of range - 3");
			
		return cells[index]; 
	}	
	function getCellsLength()
	{		
		if(cells == null)
			RetrieveCells(); 
			
		return cells.length; 
	}	
	function getChildRows()
	{
		if(childRows == -1)
		{		
			childRows = null; 
			var childRow = null; 
			var inspectedRow = null;
			var inspectedType = null; 
			var rowIndex = getRootRowFromInner().rowIndex + 1; 
			var rowLength = -1; 
			var rowTable = getGridEX().getRootTable().getHtmlItemsTable(); 
			var scanComplete = false; 
			rowLength = rowTable.rows.length; 
			do
			{
				if(rowIndex < rowLength && !scanComplete)
				{						
					inspectedRow = rowTable.rows[rowIndex]; 
					inspectedType = inspectedRow.getAttribute("type"); 
					if(inspectedType != "1" && inspectedType != "2" && inspectedType != "6" && inspectedType != "7" && inspectedType != "10")
					{
						if(inspectedRow.getAttribute("pr") == getID())	
						{
							childRow = getGridEX().RetrieveRow(inspectedRow, null, null, null); 
							if(childRows == null)
								childRows = new Array(); 

							childRows[childRows.length] = childRow; 
						}
					}					
					rowIndex++;
				}
			} while(rowIndex < rowLength && !scanComplete);
		}		
		return childRows;
	}	
	function getCurrentCell() { return currentCell;  }
	function getDataKeyValues() { return dataKeyValues; }
	function getExpanded() { return expanded; }	
	function getInnerRow() { return innerRow; }	
	function getIsAlternating() { return isAlternating; }
	function getIsChecked()
	{
		if(getRowType() != "Record")
			return false; 
			
		if(ischecked == null)
		{
			var l = getTable().getColumns().Count(); 
			for(var i=0;i<l;i++)
			{
				var column = getTable().getColumns().getGridEXColumn(i);
				if(column.getVisible() && (column.getActAsSelector() || column.getColumnType() == 4))
				{
					var innerCell = null;
					if(getInnerRow().getChildsById != null)
						innerCell = getInnerRow().getChildsById(column.getClientID() + "_L")[0]; 
					else
						innerCell = allChildsCore(column.getClientID() + "_L", getInnerRow().getElementsByTagName("*"))[0]; 
					if(innerCell != null)
					{						
						var element = getInnerSpan(innerCell.childNodes[0]).getElementsByTagName("INPUT"); 
						var k = element.length; 
						if(k > 0)
						{
							for(var j=0; j<k; j++)						
							{
								if(element[j].type == "checkbox")
									ischecked = element[j].checked; 								
							}
						}
					}
				}
			}
		}
		return ischecked; 
	}
	function getIsVisible() { return (getRootRowFromInner().style.display != "none"); }
	function getURL()
	{		
		if(getRowType() != "Record")
			return null; 
			
		return getRootRowFromInner().getAttribute("navigateto"); 		
	}
	function getURLTarget()
	{
		if(getRowType() != "Record")
			return null; 
			
		return getRootRowFromInner().getAttribute("navigatetarget");
	}
	function containsURL()
	{
		if(getRowType() != "Record")
			return false; 
	
		var _url = getURL();
		if(_url != null && _url.length > 0)
			return true;
		else
			return false; 
	}
	function getParent()
	{
		if(parentRow == -1)
		{
			if(getRootRowFromInner().getAttribute("pr") != null)
			{
				var parentInner = document.getElementById(getRootRowFromInner().getAttribute("pr")); 
				if(parentInner != null)
					parentRow = getGridEX().RetrieveRow(parentInner, null, null, null); 
				else
					parentRow = null; 
			}
			else
				parentRow = null; 
		}				
		return parentRow;		
	}	
	function getPosition() { return position; }		
	function getRowHeight()
	{
		var height = 0; 
		height = innerRow.offsetHeight;
		if(previewInnerRow != null)
			height += previewInnerRow.offsetHeight; 
		return height; 
	}	
	function getRowType()
	{
		switch(getType())
		{
			case 3:
			case 4:
				return "Record"; 
			case 5:
				return "TotalRow"; 
			case 8:
				return "GroupHeader"; 
			case 9:
				return "NewRecord"; 
			case 11:
				return "FilterRow"; 
			case 12:
				return "GroupFooter"; 			
		}
	}
	function getPreviewInnerRow() { return previewInnerRow; }	
	function getSelected()
	{
		if(getGridEX().getSelectedItems() != null)
			return getGridEX().getSelectedItems().IsRowSelected(gridEXRow); 
		return false; 
	}
	function getType() { return (type == 4) ? 3 : type; }	
	function getVisibleInScroll()
	{						
		if((getRootRowFromInner(getInnerRow()).offsetTop + getRowHeight()) >= (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetHeight  + getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop))
			return false; 
		else if((getRootRowFromInner(getInnerRow()).offsetTop - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop) < 0)
			return false;
		else
			return true; 		
	}	
	function getDataChanged()
	{
		var t = getType(); 
		if(t == 3 || t == 4 || t == 9 || t == 11)
		{
			RetrieveCells();
			var cell = null;
			var l = cells.length; 
			for(var i = 0; i < l; i++)
			{			
				cell = cells[i];
				if(cell.getDataChanged())
					return true; 
			}
		}
		return false; 
	}
	function ensureVisibleCell(cell)
	{
		var sl = getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
		var cl = getPixelLeft(cell.getInnerCell());
		var cw = cell.getInnerCell().offsetWidth;
		var tl = getPixelLeft(getGridEX().getRootTable().getHtmlItemsTable().offsetParent);
		var tw = getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth;
		if((getRowType() == "NewRecord" && getTable().getParent() == null && getTable().getNewRowPosition() == 2) || getRowType() == "FilterRow")
				cl += sl;				
		if((cl + cw - sl) > (tl + tw))
		{
			var scroll = Math.abs((cl + cw - sl) - (tl + tw));
			getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft += scroll;
			resetRootTableScroll(getGridEX().getRootTable());
		}
		else if(cl - sl < tl)
		{
			var scroll = cl - tl;
			getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft = scroll; 
			resetRootTableScroll(getGridEX().getRootTable());
		}
	}
	function setCurrentCell(cell)
	{
		currentCell = cell;
		if(getGridEX().currentCell != null)
		{
			if(getGridEX().currentCell.getInnerCell().getAttribute("align") == "center" || getGridEX().currentCell.getInnerCell().getAttribute("align") == "right" || getGridEX().currentCell.getInnerCell().style.textAlign != "")
				getGridEX().currentCell.getInnerCell().style.textAlign = ""; 			
			getGridEX().currentCell.getInnerCell().setAttribute("className", getGridEX().currentCell.cssName);
		}		
		if(currentCell != null)
		{
			currentCell.cssName = cell.getInnerCell().getAttribute("className");
			if(currentCell.getInnerCell().getAttribute("align") == "center" || currentCell.getInnerCell().getAttribute("align") == "right")
				currentCell.getInnerCell().style.textAlign = currentCell.getInnerCell().getAttribute("align"); 			
			else if(isDefaultView)
			{
				if(document.defaultView.getComputedStyle(currentCell.getInnerCell(),null).getPropertyValue("text-align") == "right" || document.defaultView.getComputedStyle(currentCell.getInnerCell(), null).getPropertyValue("text-align") == "center")
					currentCell.getInnerCell().style.textAlign = document.defaultView.getComputedStyle(currentCell.getInnerCell(),null).getPropertyValue("text-align"); 
			}				
			if(getGridEX().focusCss != null)				
				currentCell.getInnerCell().setAttribute("className", getClassName(gridEXRow) + " " + getGridEX().focusCss); 
			else
				currentCell.getInnerCell().setAttribute("className", getClassName(gridEXRow) + " " + currentCell.cssName);			
		}
		getGridEX().currentCell = currentCell;
		if(getGridEX().currentCell != null)
			getGridEX().FireEvent("CurrentCellChanged", [getGridEX().currentCell]); 
		ensureVisibleCell(currentCell); 
	}			
	function getCellSelected()
	{		
		var cell = null;
		var columnID = null;		
		var indexOf = -1; 		
		var element = window.event.srcElement;
		while(element != null && cell == null)
		{		
			if(element.nodeType == 1 && element.tagName == "TD")
			{
				columnID = element.getAttribute("id"); 
				if(columnID != null)
				{
					indexOf = columnID.indexOf("_L"); 
					if(indexOf > 0)
					{
						columnID = columnID.substring(0, indexOf); 
						cell = getCellByColumnID(columnID);
					}
				}
			}		
			element = element.parentElement;
		}		
		if(element == null || cell == null)
			return null;
			
		if(cell != null)
		{	
			var innerColumnSet = null; 
			if(getTable().getUseColumnSets())
			{				
				element = cell.getInnerCell(); 				
				while(innerColumnSet == null && element != null)
				{
					if(element.tagName == "TABLE")
						innerColumnSet = element; 
					else
						element = element.parentElement; 
				}				
				if(innerColumnSet != null)
				{
					currentColumnSet = innerColumnSet.parentElement;
					currentColumnSetRow = cell.getInnerCell().parentElement; 
				}
			}			
			return cell;			
		}		
		return null; 
	}	
	function BeforeEdit()	
	{
		var cell = getCellSelected(); 
		if(cell != null)
		{
			if(!cell.getColumn().getSelectable() || (cell.getColumn().getColumnType() == 4 && cell.getColumn().getEditType() == -1))
			{
				window.event.returnValue = false; 
				window.event.cancelBubble = true;
				return false; 				
			}
			else if(cell.getColumn().getColumnType() == 4 && cell.getColumn().getEditType() == 9)
			{
				var args = new GridEXEditingArgs(cell, cell.getValue());
				var cancel = cell.getGridEX().FireEvent("EditingCell", [args]);
				if(cancel != null && cancel)
				{
					window.event.returnValue = false;
					window.event.cancelBubble = true;
					return false; 
				}
			}
			if(cell.getColumn().getActAsSelector() && ((window.event.srcElement != null && window.event.srcElement.tagName == "INPUT" && window.event.srcElement.getAttribute("type") == "checkbox") || (document.activeElement != null && document.activeElement.tagName == "INPUT" && document.activeElement.getAttribute("type") == "checkbox"))) 
				CheckRow(cell.getValue(),cell.getColumn().getClientID(), true); 
			else
			{	
				setCurrentCell(cell); 		
				cell.ShowEditorCell(); 				
			}
		}			
	}	
	function BeforeFilter()
	{	
		if(getType() != 11)
			return; 
			
		var cell = getCellSelected(); 
		if(cell != null)
		{
			if(!cell.getColumn().getSelectable())
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
				return false; 
			}			
			cell.ShowFilterCell(); 			
			setCurrentCell(cell); 
		}
	}
	function CheckRow(checked,colID, reviewStatus, fireEvent)
	{
		if(getType() != 3)
			return; /* throw Error("invalid operation exception"); */
			
		if(cells == null)
		{
			var l =	getTable().getColumns().Count();
			for(var i=0;i<l;i++)
			{
				var column = getTable().getColumns().getGridEXColumn(i);
				if((colID == null && column.getActAsSelector() || column.getColumnType() == 4) || (column.getClientID() != colID && column.getActAsSelector() && column.getColumnType() == 4))
				{
					var innerCell = null; 
					if(getInnerRow().getChildsById != null)
						innerCell = getInnerRow().getChildsById(column.getClientID() + "_L")[0]; 
					else
						innerCell = allChildsCore(column.getClientID() + "_L", getInnerRow().getElementsByTagName("*"))[0];
					if(innerCell != null)
					{						
						var element = getInnerSpan(innerCell.childNodes[0]).getElementsByTagName("INPUT"); 
						var k = element.length; 
						if(k > 0)
						{
							for(var j=0;j<k;j++)
							{
								if(element[j].type == "checkbox")
									element[j].checked = checked;
							}
						}
					}
				}			
			}
		}
		else
		{
			var cell = null; 
			var l = cells.length; 
			for(var i = 0; i < l; i++)
			{
				cell = cells[i]; 
				if((colID == null && cell.getColumn().getActAsSelector() || cell.getColumn().getColumnType() == 4) || (cell.getColumn().getClientID() != colID && cell.getColumn().getActAsSelector() && cell.getColumn().getColumnType() == 4))
					cell.setValue(checked);
			}
		}
		getGridEX().FireEvent("RowCheckedChanged", [checked, null, gridEXRow]);
		ischecked = checked; 
		if(!checked)
		{
			var headers = getTable().getHeaders(); 
			if(headers != null)
			{
				var l = headers.length;
				if(l > 0)
				{
					for(var i=0; i<l; i++)
						headers[i].CheckSelectors(null, false, false);
				}
				else
				{
					if(headers.CheckSelectors != null)
						headers.CheckSelectors(null, false, false);
				}
			}
		}
		else if(checked && (reviewStatus != null && reviewStatus))
		{
			var allchecked = true; 
			var irow = 0;			
			var row = null; 
			var rowschecked = 0; 
			var recordscount = getTable().getRecordsCount(); 
			var rowscount = getGridEX().getRowsInPageCount(); 
			while(irow < rowscount && rowschecked != recordscount && allchecked)	
			{
				row = getGridEX().getRow(irow);
				if(row.getTable().getID() == getTable().getID())
				{
					if(row.getType() == 3)
					{
						if(row.getIsChecked())
						{
							rowschecked++;
							irow++;
						}
						else
							allchecked = false; 
					}
					else
						irow++; 
				}
				else
					irow++; 
			}
			if(rowschecked == recordscount && allchecked)			
			{
				var headers = getTable().getHeaders(); 
				if(headers != null)
				{
					var l = headers.length; 
					if(l > 0)
					{						
						for(var i = 0; i < l; i++)
							headers[i].CheckSelectors(null, true, false);
					}
					else
					{
						if(headers.CheckSelectors != null)
							headers.CheckSelectors(null, true, false);
					}
				}
			}
		}
		if(reviewStatus != null && reviewStatus)
			getGridEX().ReportRowsStatus(); 
		if(fireEvent != null && fireEvent)
		{
			cell = getCellSelected();
			if(cell != null && cell.getColumn().getSelectable() && cell.getColumn().getActAsSelector())
			{
				if(window.event.srcElement != null && window.event.srcElement.tagName == "INPUT" && window.event.srcElement.type == "checkbox")
				{
					if(window.event.srcElement.getAttribute("se") != null)
					{
						var argument = "RowCheckedChanged"; 
						if(checked)
							argument += ":1";
						else
							argument += ":2";							
						argument += ":" + getID(); 
						getGridEX().DoPostBack(null, argument); 
					}
				}
			}
		}	
	}
	function Collapsing()
	{
		if(getType() == 8 || (getType() == 3 && getTable().getIsParentTable()))
		{					
			var cancelEvent = getGridEX().FireEvent("CollapsingRow", [gridEXRow]);
			if(cancelEvent == null || !cancelEvent)
			{				
				if(getGridEX().getChildLoadingMode() == 1)
				{			
					var status = getRootRowFromInner(getInnerRow()).getAttribute("status"); 
					if(status != null)
					{	
						if(getGridEX().selpb)
						{
							if(getGridEX().getSelectedItems() != null && getGridEX().getSelectedItems().Count() > 0 && getGridEX().getSelectedItems().getSelectedItemInIndex(0).getRow() != gridEXRow)
								getGridEX().DoPostBack(null, "CollapseAndSelect:"+getID()); 
							else
								getGridEX().DoPostBack(null, "Collapse:"+getID()); 
								
							window.event.cancelBubble = true;
							window.event.returnValue = false;
							return false;
						}
						else
							getGridEX().DoPostBack(null, "Collapse:"+getID()); 
					}
					else
						getGridEX().MovePrevious(); 
				}
				else
				{
					if(expanded)
					{
						var headersToResize = new Array(); 
						toogleRows(getRootRowFromInner().rowIndex+1, getGridEX().getRootTable().getHtmlItemsTable(), getID(), getTable().getID(), 0, headersToResize);
						resizeHeadersAfterToogle(headersToResize); 
						toogleIndicator(1);
						getRootRowFromInner().removeAttribute("status"); 
						expanded = false;
						getGridEX().FireEvent("RowCollapsed", [gridEXRow]); 
					}
					else
						getGridEX().MovePrevious(); 
				}
				getGridEX().ReportRowsStatus(); 
			}
		}
	}	
	function CollapsePreviewRow(innerCell)
	{
		var isExpanded = true; 
		isExpanded = (previewInnerRow.style.display != "none") ? true : false; 		
		if(isExpanded)
			previewInnerRow.style.display = "none"; 
		else
			previewInnerRow.style.display = ""; 	// hack!
		if(isExpanded)
			innerCell.childNodes[0].childNodes[1].src = getTable().getCollapsedPreviewRowGlyph(); 
		else
			innerCell.childNodes[0].childNodes[1].src = getTable().getExpandedPreviewRowGlyph(); 
		getGridEX().ReportRowsStatus(); 
	}	
	function getRowHeaderCell()
	{
		if(rowHeaderCell)
			return rowHeaderCell; 			
			
		var cell = null; 
		var cellHeader = null; 
		var cellIndex = 0; 
		var _cellsLength = -1;
		_cellsLength = getInnerRow().cells.length; 
		while(cellHeader == null && cellIndex < _cellsLength)
		{
			cell = getInnerRow().cells[cellIndex]; 
			if(cell.getAttribute("type") == "rh")
				cellHeader = cell; 
			cellIndex++; 
		}		
		return cellHeader; 
	}	
	function getHeaderSettings()
	{
		var headerLeft = -1; 
		var headerTop = -1; 
		var headerHeight = 0; 
		var headerWidth = 0; 		
		var cellHeader = getRowHeaderCell();				
		if(cellHeader == null) 
			return null; 
			
		headerTop = getPixelTop(cellHeader) - getVerticalScrollOffset(getGridEX()); 
		headerLeft = getPixelLeft(cellHeader) - getHorizontalScrollOffset(getGridEX());
		headerWidth = cellHeader.offsetWidth;
		headerHeight = getInnerRow().offsetHeight; 							
		if(getPreviewInnerRow() != null)
			headerHeight += getPreviewInnerRow().offsetHeight; 

		return [headerLeft, headerTop, headerWidth, headerHeight]; 		
	}	
	function HideHeaderIndicator()
	{
		hideCurrentRowHeader(); 		
		if(getTable().getRowHeaders())
		{
			if(getType() == 9)
			{
				showHeaderIndicatorCore(getGridEX().rowheaders[2]); 				
				currentRowHeader = null;
			}
		}
	}
	function ReportStatus()
	{		
		var rowstatus = "";
		rowstatus += getID() + ",";
		rowstatus += (getExpanded() ? "1" : "0") + ",";
		if(getType() == 3)
		{
			if(getPreviewInnerRow() != null)
				rowstatus += (getPreviewInnerRow().style.display != "none" ? "1" : "0") + ",";  
			else
				rowstatus += "-1,"; 			
			if(getIsChecked())
				rowstatus += "1,";
			else
				rowstatus += "0,";
		}
		else
		{
			rowstatus += "-1," 
			rowstatus += "-1,"; 
		}
		if(getSelected())
			rowstatus += "1,"; 
		else
			rowstatus += "-1,"; 
		if(this.current)
			rowstatus += "1";
		else
			rowstatus += "-1"; 
		return rowstatus; 
	}
	function hideCurrentRowHeader()
	{
		if(currentRowHeader != null)
			currentRowHeader.getElementsByTagName("DIV")[0].style.backgroundImage = "none"; 
	}
	function showHeaderIndicatorCore(img)
	{
		var s = getRowHeaderCell().getElementsByTagName("DIV")[0]; 
		s.style.backgroundImage = "url(" +  img +")"; 
		s.style.backgroundPosition = "center center"; 
		s.style.backgroundRepeat = "no-repeat"; 
		currentRowHeader = getRowHeaderCell(); 
	}
	function ShowHeaderIndicator(editing)
	{
		if(getTable().getRowHeaders())				
		{							
			if(getType() == 5 || getType() == 8 || getType() == 11 || getType() == 12) 
				return; 
		
			if((getType() == 3 || getType() == 4 || getType() == 9) && ((editing != null && editing) || (getDataChanged() && getTable().getAllowEdit())))
			{
				if(headerIndicatorType == -1 || headerIndicatorType == 1)
				{
					hideCurrentRowHeader(); 
					if(getRowHeaderCell() != null)
						showHeaderIndicatorCore(getGridEX().rowheaders[1]); 						
					headerIndicatorType = 2; 					
					return; 
				}				
				else if(headerIndicatorType == 2)
				{					
					hideCurrentRowHeader(); 
					if(getRowHeaderCell() != null)
						showHeaderIndicatorCore(getGridEX().rowheaders[1]);
						
					return; 
				}
			}
			else
			{
				if(headerIndicatorType == -1 || headerIndicatorType == 2)
				{					
					hideCurrentRowHeader();
					if(getRowHeaderCell() != null)
						showHeaderIndicatorCore(getGridEX().rowheaders[0]);
						
					headerIndicatorType = 1; 
					return; 
				}
				else if(headerIndicatorType == 1)
				{					
					hideCurrentRowHeader();
					if(getRowHeaderCell() != null)
						showHeaderIndicatorCore(getGridEX().rowheaders[0]); 					
					return; 
				}				
			}
		}
	}	
	function getNextItem(i, rowTable)
	{
		var inspectedRow = null;
		var inspectedType = null;
		var l = rowTable.rows.length; 
		while(i < l)
		{
			inspectedRow = rowTable.rows[i]; 
			inspectedType = inspectedRow.getAttribute("type"); 
			if(inspectedType != "7" && inspectedType != "10")
				return inspectedRow;			
				
			i++; 
		}
		return null; 
	}	
	function getNextItemID(item, rowTable)
	{
		var inspectedType = null; 
		inspectedType = item.getAttribute("type"); 
		if(inspectedType != "1" && inspectedType != "2" && inspectedType != "4" && inspectedType != "7" && inspectedType != "10")
			return item; 
		else if(item.rowIndex + 1 < rowTable.rows.length)
			return getNextItemID(rowTable.rows[item.rowIndex+1], rowTable);
		else
			return null;
	}	
	function getPreviousItem(rowIndex, rowTable)
	{
		var inspectedRow = null; 
		var inspectedType = null; 
		while(rowIndex >= 0)
		{		
			inspectedRow = rowTable.rows[rowIndex]; 
			inspectedType = inspectedRow.getAttribute("type"); 
			if(inspectedType != "2" && inspectedType != "1" && inspectedType != "4" && inspectedType != "7" && inspectedType != "10")
				return inspectedRow;
				
			rowIndex--; 			
		}
		return null; 
	}
	function toogleCoreIndicator(_tmpCells, _tmpCellsLength, action)
	{
		var _img = null; 
		var _cell = null; 
		var _spans = null; 
		for(var _tmpCell = 0; _tmpCell < _tmpCellsLength; _tmpCell++)
		{
			_cell = _tmpCells[_tmpCell];							
			if(getTable().getHierarchicalMode() == 1 || getType() == 8)
			{			
				if(_cell.getAttribute("name") == "ec")
				{
					if(_cell.getElementsByTagName("DIV").length > 0)
						_img = _cell.getElementsByTagName("DIV")[0].getElementsByTagName("IMG")[0]; 
					else if(_cell.getElementsByTagName("SPAN").length > 0)
						_img = _cell.getElementsByTagName("SPAN")[0].getElementsByTagName("IMG")[0]; 											
					
					if(_img.getAttribute("unselectable") != null && _img.getAttribute("unselectable") != "")
						return; 
				}
			}
			else if(getTable().getHierarchicalMode() == 2 && getType() == 3)
			{
				if(_cell.getAttribute("type") == "ec")
				{					
					if(_cell.getElementsByTagName("DIV").length > 0)
						_img = _cell.getElementsByTagName("DIV")[0].getElementsByTagName("IMG")[0];
					else if(_cell.getElementsByTagName("SPAN").length > 0)
						_img = _cell.getElementsByTagName("SPAN")[0].getElementsByTagName("IMG")[0];					
					
					if(_img.getAttribute("unselectable") != null && _img.getAttribute("unselectable") != "")
						return; 
				}
			}			
			if(getRowType() == "GroupHeader")
			{			
				var _indexof = _cell.className.indexOf("HJ");
				if(_indexof > 0)
					_cell.className = _cell.className.substr(0, _indexof) + "HA"; 
				else
				{
					_indexof = _cell.className.indexOf("HA"); 
					if(_indexof > 0)
						_cell.className = _cell.className.substr(0, _indexof) + "HJ"; 
				}
			}						
			if(_img != null)
			{	
				if(action == 0)
				{
					if(getType() == 8)
					{
						var src = eval("groupCollapse" + getGridEX().getID());
						if(src != null && src.length != 0)
							_img.src = src; 
						else
							_img.src = "/aspnet_client/janus_web_gridex/images/jsopen.gif";
					}
					else
					{
						var src = eval("recordCollapse" + getGridEX().getID()); 
						if(src != null && src.length != 0)
							_img.src = src;
						else
							_img.src = "/aspnet_client/janus_web_gridex/images/jsopen.gif"; 						
					}
				}
				else
				{	
					if(getType() == 8)
					{
						var src = eval("groupExpand" + getGridEX().getID()); 
						if(src != null && src.length != 0)
							_img.src = src; 
						else
							_img.src = "/aspnet_client/janus_web_gridex/images/jsclosed.gif"; 
					}
					else
					{
						var src = eval("recordExpand" + getGridEX().getID()); 
						if(src != null && src.length != 0)
							_img.src = src; 
						else
							_img.src = "/aspnet_client/janus_web_gridex/images/jsclosed.gif"; 
					}
				}
				return true; 
			}			
		}
		return false; 
	}
	function toogleIndicator(action, raiseError)
	{		
		var _cell = null;		
		var _tmpCellsLength = -1; 
		var _tmpTable = null;
		var _tmpRow = null; 		
		if(getType() == 8)
		{
			_tmpRow = getInnerRow(); 		
			cellElement = null;
			cellsLength = _tmpRow.cells.length; 
			icell = 0; 
			while(cellElement == null && icell < cellsLength)
			{
				if(_tmpRow.cells[icell].childNodes.length == 1 && _tmpRow.cells[icell].childNodes[0].tagName == "TABLE")
						cellElement = _tmpRow.cells[icell].childNodes[0]; 
					else
						icell++; 
			}
			if(cellElement == null)
			{
				if(raiseError == null || raiseError)
					throw Error("unable to find cell"); 
				else
					return;
			}
			_tmpRow = cellElement.rows[0]; 
		}
		else
		{	
			_tmpTable = getRootRowFromInner().cells[0].childNodes[0]; 
			_tmpRow = _tmpTable.rows[0];
			if(getTable().getHierarchicalMode() == 2 && getType() == 3)
			{				
				var cellElement = null; 
				var cellsLength = _tmpRow.cells.length;
				var icell = 0; 
				while(cellElement == null && icell < cellsLength)
				{
					if(_tmpRow.cells[icell].childNodes.length == 1 && _tmpRow.cells[icell].childNodes[0].tagName == "TABLE")
						cellElement = _tmpRow.cells[icell].childNodes[0]; 
					else
						icell++; 
				}				
				if(cellElement == null)
				{
					if(raiseError == null || raiseError)
						throw Error("unable to find cell"); 
					else
						return; 
				}					
				_tmpTable = cellElement; 
				_tmpRow = _tmpTable.rows[0]; 
			}
		}		
		_tmpCellsLength = _tmpRow.cells.length; 		
		if(getRowType() == "Record" && getTable().getUseColumnSets())
		{
			var l = -1;
			for(var j = 0; j < _tmpCellsLength; j++)
			{
				_cell = _tmpRow.cells[j];				
				if(_cell.getAttribute("type") != "rh" && _cell.childNodes.length > 0 && _cell.childNodes[0].tagName == "TABLE")
				{				
					l = _cell.childNodes[0].rows[0].cells.length; 					
					for(var i = 0; i < l; i++)			
					{
						if(_cell.childNodes[0].rows[0].cells[i].getAttribute("type") != "rh")
						{
							var x = _cell.childNodes[0].rows[0].cells[i].getElementsByTagName("TABLE")[0].cells;
							if(x == null)
								x = getCellsCore(_cell.childNodes[0].rows[0].cells[i].getElementsByTagName("TABLE")[0]);
							if(toogleCoreIndicator(x, x.length, action) == true)
								return; 
						}
					}
				}
			}
		}
		else
			toogleCoreIndicator(_tmpRow.cells, _tmpCellsLength, action);		
	}
	var lastVisibleInspectedRow = -1;
	function toogleRows(rowIndex, rowTable, parentRowID, parentTableID, action, headersToResize, commitAction, expandAll)
	{	
		var _selfparent = null;
		var inspectedRow = null; 
		var inspectedType = null; 
		var rowsLength = rowTable.rows.length; 
		var scanComplete = false; 
		var _table = getGridEX().getTables().getTableByID(parentTableID);
		var _modifiedrow = null;
		if(_table.getHierarchicalMode() == 2)
			_selfparent = rowTable.rows[rowIndex-1]; 			
		do
		{
			if(rowIndex < rowsLength)
			{	
				inspectedRow = rowTable.rows[rowIndex]; 
				inspectedType = inspectedRow.getAttribute("type"); 
				if(inspectedRow.getAttribute("pr") == parentRowID)
				{	
					if(action == 1)
					{
						if(inspectedType == "1" || inspectedType == "2" || inspectedType == "7")
						{
							if(lastVisibleInspectedRow < rowsLength && (rowTable.rows[lastVisibleInspectedRow].getAttribute("t") != inspectedRow.getAttribute("t")))
							{	
								if(expandAll != null && expandAll)
								{
									inspectedRow.setAttribute("status", "1");
									var _arow = getGridEX().RetrieveRow(inspectedRow, null, null, null, null);
									_arow.toogleIndicator(action == 1 ? 0 : 1, false); 
								}
								inspectedRow.style.display = "";  								
								inspectedRow.parentElement.style.width = inspectedRow.parentElement.offsetWidth;
								if(inspectedType == "1" && getGridEX().getColumnAutoResize())
								{	
									headersToResize[headersToResize.length] = inspectedRow.getAttribute("t"); 
									headersToResize[headersToResize.length] = inspectedRow.rowIndex;
								}
								_modifiedrow = inspectedRow;
							}
						}
						else
						{			
							if(expandAll != null && expandAll)
							{
								inspectedRow.setAttribute("status", "1");
								var _arow = getGridEX().RetrieveRow(inspectedRow, null, null, null, null);
								_arow.toogleIndicator(action == 1 ? 0 : 1, false); 
							}
							if(commitAction == null || !commitAction)				
								inspectedRow.style.display = "";
							inspectedRow.parentElement.style.width = inspectedRow.parentElement.offsetWidth;					
							lastVisibleInspectedRow = inspectedRow.rowIndex; 
							_modifiedrow = inspectedRow;
						}
					}
					else
					{						
						inspectedRow.style.display = "none";
						lastVisibleInspectedRow = inspectedRow.rowIndex; 
						_modifiedrow = inspectedRow;
					}
					if(inspectedRow.getAttribute("status") != null || inspectedRow.getAttribute("type") == "8")
					{
						lastVisibleInspectedRow = inspectedRow.rowIndex; 
						inspectedRow = toogleRows(inspectedRow.rowIndex+1, rowTable, inspectedRow.getAttribute("id"), inspectedRow.getAttribute("t"), action, headersToResize, inspectedRow.getAttribute("status") == null, expandAll); 
						if(inspectedRow != null)
							rowIndex = inspectedRow.rowIndex;
						else
							rowIndex = lastVisibleInspectedRow + 1;
					}
					else
						rowIndex++;
				}				
				else if(inspectedRow.getAttribute("t") == parentTableID)
				{	
					if(_table.getHierarchicalMode() == 1)
					{
						if(inspectedRow.getAttribute("type") == "1" || inspectedRow.getAttribute("type") == "2" || inspectedRow.getAttribute("type") == 7)
						{	
							if(lastVisibleInspectedRow < rowsLength && (rowTable.rows[lastVisibleInspectedRow].getAttribute("t") != inspectedRow.getAttribute("t")))
							{
								var _offsetRow = inspectedRow; 
								while(_offsetRow.getAttribute("type") == "1" || _offsetRow.getAttribute("type") == "2" || _offsetRow.getAttribute("type") == "7")
								{
									if(action == 1)
									{
										_offsetRow.style.display = "";
										_offsetRow.parentElement.style.width = _offsetRow.parentElement.offsetWidth; 
										if(_offsetRow.getAttribute("type") == "1")
										{
											headersToResize[headersToResize.length] = inspectedRow.getAttribute("t"); 
											headersToResize[headersToResize.length] = _offsetRow.rowIndex;
										}
									}
									else
										_offsetRow.style.display = "none"; 
													
									_offsetRow = rowTable.rows[_offsetRow.rowIndex+1]; 
								}
								inspectedRow = _offsetRow; 
							}
							return inspectedRow; 														
						}						
						else
							return inspectedRow; 
					}
					else
					{
						if(_selfparent != null && inspectedRow.getAttribute("pr") == _selfparent.getAttribute("pr"))
							return inspectedRow;
						else
							rowIndex++; 
					}
				}				
				else									
				{	
					if(inspectedRow.getAttribute("t") != null)
					{
						if(getGridEX().getTables().getTableByID(inspectedRow.getAttribute("t")).IsParentOf(parentTableID))						
							return inspectedRow; 						
					}
					rowIndex++; 					
				}				
			}
		}	while(rowIndex < rowsLength); 				
		return _modifiedrow; 	
	}	
	function resizeHeadersAfterToogle(headersToResize)
	{
		if(getGridEX().getColumnAutoResize() && headersToResize.length > 0)
		{
			var resizeTable = null;			
			for(var iHeaderResize = 0; iHeaderResize < headersToResize.length; iHeaderResize = iHeaderResize + 2)
			{
				if(resizeTable != null)
				{
					if(headersToResize[iHeaderResize] != resizeTable.getID())
						resizeTable = getGridEX().getTables().getTableByID(headersToResize[iHeaderResize]);			
				}
				else
					resizeTable = getGridEX().getTables().getTableByID(headersToResize[iHeaderResize]);
					
				resizeTable.ResizeHeaderInRow(headersToResize[iHeaderResize+1]); 
			}
		}
	}
	function Expanding()
	{		
		if(getType() == 8 || (getType() == 3 && getTable().getIsParentTable()))
		{
			var cancelEvent = null; 			
			cancelEvent = getGridEX().FireEvent("ExpandingRow", [gridEXRow]);
			if(getGridEX().getChildLoadingMode() == 1)
			{			
				if(cancelEvent == null || !cancelEvent)
				{
					if(getGridEX().selpb)
					{
						if(getGridEX().getSelectedItems() != null && getGridEX().getSelectedItems().Count() > 0 && getGridEX().getSelectedItems().getSelectedItemInIndex(0).getRow() != gridEXRow)
							getGridEX().DoPostBack(null, "ExpandAndSelect:"+getID()); 
						else	
							getGridEX().DoPostBack(null, "Expand:"+getID());
							
						window.event.cancelBubble = true;
						window.event.returnValue = false;
						return false; 
					}		
					else
						getGridEX().DoPostBack(null, "Expand:"+getID()); 					
				}
			}
			else				
			{	
				if(cancelEvent == null || (typeof(cancelEvent) == "boolean" && !cancelEvent) || typeof(cancelEvent) == "number") 
				{
					if(!expanded)
					{					
						var headersToResize = new Array(); 
						lastVisibleInspectedRow = getRootRowFromInner().rowIndex; 
						toogleRows(getRootRowFromInner().rowIndex+1, getGridEX().getRootTable().getHtmlItemsTable(), getID(), getTable().getID(), 1, headersToResize, null, cancelEvent == 3 ? true : false);
						resizeHeadersAfterToogle(headersToResize); 
						toogleIndicator(0); 
						getRootRowFromInner().setAttribute("status", "1"); 
						expanded = true;				
						getGridEX().FireEvent("RowExpanded", [gridEXRow]); 
					}
					else
						MoveNext();
				}
			}
		}
	}
	function ensureVisibleRow(row)
	{
		if(!row.getVisibleInScroll())
		{			
			if(getRootRowFromInner(getInnerRow()).offsetTop + row.getRowHeight() >= 	getGridEX().getRootTable().getHtmlItemsTable().offsetHeight)
				getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop = 0; 
			else
			{			
				var scrollTop = getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop; 
				getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop = scrollTop +  row.getRowHeight(); 
			}
		}
	}
	function tabToCell(colID, editing)
	{	
		var cell = getCellByColumnID(colID); 
		if(editing != null && editing)
		{
			if(getType() == 11)
				cell.ShowFilterCell(); 
			else
				cell.ShowEditorCell();
		}								
		setCurrentCell(cell); 
	}	
	function getNextTabCellIndex(id, direction)
	{
		if(direction == null || direction == 1)
		{			
			for(var i=0;i<getTable().tabCellsOrder.length;i=i+2)
			{
				if(getTable().tabCellsOrder[i+1] == id)
				{
					if(i+2<getTable().tabCellsOrder.length)
						return i+2;
					else
						return -1; 
				}
			}
			return -1;
		}
		else
		{
			for(var i=getTable().tabCellsOrder.length-2;i>=0;i=i-2)
			{
				if(getTable().tabCellsOrder[i+1] == id)
				{
					if(i-2 >= 0)
						return i-2;
					else
						return -1;
				}
			}
			return -1; 
		}
	}
	function setFocusCell(x)
	{
		if(getTable().tabCellsOrder != null && getTable().tabCellsOrder.length > 0)
		{
			var i = getNextTabCellIndex(currentCell.getColumn().getClientID(), x);
			if(i != -1)
				setCurrentCell(getCellByColumnID(getTable().tabCellsOrder[i+1]));
			else
			{
				var row = null;
				if(x == -1)
					row = getPreviousRow();
				else
					row = getNextRow();
				if(row != null)
				{
					getGridEX().setCurrentRow(row);
					getGridEX().getSelectedItems().SelectSingleRow(row);
					ensureVisibleRow(row); 
					if(row.getRowType() == "Record" || row.getRowType() == "NewRecord" || row.getRowType() == "FilterRow")
					{
						if(row.getTable().tabCellsOrder != null && row.getTable().tabCellsOrder.length > 0)
						{
							if(x == -1)
								row.setCurrentCell(row.getCellByColumnID(row.getTable().tabCellsOrder[row.getTable().tabCellsOrder.length-1])); 
							else
								row.setCurrentCell(row.getCellByColumnID(row.getTable().tabCellsOrder[1]));
						}
					}
				}
			}			
		}
	}
	function NextFocusCell()
	{
		if(currentCell != null)
			setFocusCell(1);			
	}
	function PreviousFocusCell()
	{
		if(currentCell != null)
			setFocusCell(-1); 
	}
	function TabCellChanged(editing)
	{		
		var row = null;
		if(currentCell != null)
		{
			if(window.event.shiftKey)
			{
				if(getTable().tabCellsOrder != null && getTable().tabCellsOrder.length > 0)
				{
					var i = getNextTabCellIndex(currentCell.getColumn().getClientID(), -1);
					if(i != -1)
						tabToCell(getTable().tabCellsOrder[i+1],editing);
					else
					{
						row = getPreviousRow();
						if(row != null)
						{
							getGridEX().setCurrentRow(row);
							getGridEX().getSelectedItems().SelectSingleRow(row);
							ensureVisibleRow(row);
							row.TabChanged(editing);
						}
						else
						{
							if(getRowType() == "NewRecord" && getDataChanged())	
								getGridEX().ResumeEditOperation();
						}
					}
				}
			}
			else
			{
				if(getTable().tabCellsOrder != null && getTable().tabCellsOrder.length > 0)
				{
					var i = getNextTabCellIndex(currentCell.getColumn().getClientID());
					if(i != -1)
						tabToCell(getTable().tabCellsOrder[i+1], editing); 
					else
					{
						row = getNextRow(); 
						if(row != null)
						{					
							getGridEX().setCurrentRow(row); 
							getGridEX().getSelectedItems().SelectSingleRow(row);
							ensureVisibleRow(row);
							row.TabChanged(editing);
						}
						else
						{
							if(getRowType() == "NewRecord" && getDataChanged())	
								getGridEX().ResumeEditOperation();
						}
					}
				}						
			}
		}
		else
		{
			if(window.event.shiftKey)
			{
				if(getTable().tabCellsOrder != null && getTable().tabCellsOrder.length > 0)
					tabToCell(getTable().tabCellsOrder[getTable().tabCellsOrder.length-1], editing); 
			}
			else
			{
				if(getTable().tabCellsOrder != null && getTable().tabCellsOrder.length > 0)						
					tabToCell(getTable().tabCellsOrder[1], editing);	
			}				
		}
	}	
	function TabChanged(editing)
	{
		currentCell = null;
		currentColumnSet = null;
		currentColumnSetRow = null; 
		if(getType() == 3 || getType() == 4 || getType() == 9 || getType() == 11)			 
			TabCellChanged(editing);
		else
			getInnerRow().cells[0].focus(); 
	}	
	function TabChanging(editing)
	{	
		if((getType() == 3 || getType() == 4 || getType() == 9 || getType() == 11) && (editing != null && editing))
			TabCellChanged(editing); 
		else
		{	
			var row = getNextRow(); 
			if(row != null)
			{		
				getGridEX().setCurrentRow(row); 
				getGridEX().getSelectedItems().SelectSingleRow(row);					
				if(!row.getVisibleInScroll())
				{
					if(getRootRowFromInner(getInnerRow()).offsetTop + row.getRowHeight() >= getGridEX().getRootTable().getHtmlItemsTable().offsetHeight)
						getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop = 0; 
					else
					{
						var scrollTop = getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop;
						getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop = scrollTop +  row.getRowHeight(); 
					}
				}
				row.TabChanged(editing); 
			}
		}
	}		
	function getColumnSetsCount() { return columnSetsCount; }	
	function getColumnSetInIndex(columnSetIndex)
	{		
		var columnSet = null; 
		var columnSetOffset = 0; 
		for(var i = 0; i < innerRow.cells.length; i++)
		{
			if(innerRow.cells[i].childNodes.length > 0 && innerRow.cells[i].childNodes[0].tagName == "TABLE")
			{
				if(columnSetOffset == columnSetIndex)
					return innerRow.cells[i];
					
				columnSetOffset++; 
			}			
		}		
		throw Error("argument out of range");		
	}	
	function getRootRowFromInner(element)
	{		
		if(element == null && rootInnerRow != null)
			return rootInnerRow;
	
		if(rootInnerRow == null)
		{	
			while(element != null)
			{
				if(element.nodeType == 1 && element.tagName == "TR" && element.getAttribute("id") != null && element.getAttribute("t") != null)
					return element; 
			
				element = element.parentNode; 
			}
			if(element == null)
				throw Error("unable to find root row");
				
			rootInnerRow = element; 
		}		
		return rootInnerRow; 
	}	
	function getInnerItemRow(row)
	{
		var _innerItemRow = null; 
		var _tableID = null; 
		if(getGridEX().isHierarchicalGrid())
		{			
			var _tmpTable = row.cells[0].childNodes[0]; 
			var _tmpRow = _tmpTable.rows[0]; 
			var _tmpCellsLength = _tmpRow.cells.length; 
			for(var _tmpCell = 0; _tmpCell < _tmpCellsLength && _innerItemRow == null; _tmpCell++)
			{
				if(_tmpRow.cells[_tmpCell].childNodes[0].tagName == "TABLE")
					_innerItemRow = _tmpRow.cells[_tmpCell].childNodes[0].rows[0]; 
			}			
			if(_innerItemRow == null)			
				throw Error("unable to retrieve inner row"); 
		}		
		if(row.getAttribute("t") != null)
			_tableID = row.getAttribute("t"); 
			
		if(_tableID != null && getGridEX().getTables().getTableByID(_tableID).getUseColumnSets())
		{
			if(_innerItemRow == null)
				_innerItemRow = row;
				
			if(getType() == 3 || getType() == 9)				
				return _innerItemRow.cells[0].childNodes[0].rows[0]; 
			else
				return _innerItemRow; 
		}
		else if(getGridEX().isHierarchicalGrid() && _innerItemRow != null)
			return _innerItemRow;
		else
			return row;
	}	
	function getPreviousRow()
	{
		var previousGridEXRow = null;				
		if(getPosition() - 1 >= 0)			
		{
			if(getGridEX().getChildLoadingMode() == 1)
				previousGridEXRow = getGridEX().getRow(getPosition() - 1);
			else			
			{
				if(getType() == 9 && getTable().getParent() == null && getTable().getNewRowPosition() == 2)
				{
					if(getPosition() == 1)
						previousGridEXRow = getGridEX().getFilterRow(); 
				}
				else
					previousGridEXRow = getGridEX().getPreviousVisibleRow(getRootRowFromInner().rowIndex-1,getPosition()-1);
			}
		}
		return previousGridEXRow; 
	}	
	function getNextRow()
	{	
		var nextGridEXRow = null;	
		if(getRowType() == "NewRecord" && getPosition() == -1 && getGridEX().getRowsInPageCount() == 1)
			return null;
		if(getPosition() + 1 < getGridEX().getRowsInPageCount())		
		{
			if(getGridEX().getChildLoadingMode() == 1)
				nextGridEXRow = getGridEX().getRow(getPosition() + 1);
			else
			{
				if(getType() == 11 && getTable().getParent() == null)
				{
					if(getTable().getNewRowPosition() == 2)	
						nextGridEXRow = getGridEX().getNewRecord(); 
					else
						nextGridEXRow = getGridEX().getNextVisibleRow(0,getPosition()+1);						
				}				
				else if(getType() == 9 && getTable().getParent() == null && getTable().getNewRowPosition() == 2)
					nextGridEXRow = getGridEX().getNextVisibleRow(0,getPosition()+1);
				else
					nextGridEXRow = getGridEX().getNextVisibleRow(getRootRowFromInner().rowIndex+1,getPosition()+1); 					
			}
		}
		return nextGridEXRow; 
	}
	function RetrieveCells()
	{
		if(cells == null)
		{
			if(type == 3 || type == 4 || type == 5 || type == 9 || type == 11)
				cells = new Array();		
			if(getType() == 3 || getType() == 4 || getType() == 5 || getType() == 9 || getType() == 11)
			{	
				var column = null; 
				var l = getTable().getColumns().Count(); 	
				for(var i = 0; i < l; i++)
				{
					column = getTable().getColumns().getGridEXColumn(i);
					cells[i] = new GridEXCell(column, gridEXRow); 
				}
			}
		}		
	}
	if(rootInnerRow == null)
		rootInnerRow = getRootRowFromInner(getInnerRow()); 	
	if(getType() == 3 || getType() == 8)
		expanded = (getRootRowFromInner().getAttribute("status") != null) ? true : false;
	if(getRowType() == "Record")
	{
		if(rootInnerRow.getAttribute("keys") != null)
			dataKeyValues = rootInnerRow.getAttribute("keys").split("$"); 			
	}		
	if(type == 4)
	{		
		if(getGridEX().isHierarchicalGrid())
		{
			if(getTable().getUseColumnSets())
			{
				if(innerRow.offsetParent != null)
					previewInnerRow = innerRow.offsetParent.offsetParent.offsetParent.rows[1]; 
				else
				{
					var _t = getTableOffsetParent(innerRow); 					
					if(_t != null)					
						previewInnerRow = _t.rows[1];
				}
			}
			else
			{
				if(innerRow.offsetParent != null)				
					previewInnerRow = innerRow.offsetParent.rows[innerRow.rowIndex+1];
				else
				{
					var _t = innerRow.parentElement;
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
					if(_t != null)
						previewInnerRow = _t.rows[innerRow.rowIndex+1];
				}
			}
		}			
		else
			previewInnerRow = getTable().getHtmlItemsTable().rows[getRootRowFromInner().rowIndex+1]; 			
	}
	if(position == -1)
	{		
		if(getType() == 9 || getType() == 11 && getTable().getParent() == null)
		{
			if(getType() == 11)
				position = 0; 
			else if(getGridEX().getFilterMode() == 1 && getTable().getNewRowPosition() == 2)
				position = 1;
			else if(getGridEX().getFilterMode() != -1 && getTable().getNewRowPosition() == 2)
				position = 0; 			
		}
		else
		{		
			var _rootRow = null; 
			var _indexPos = 0; 
			var l = getTable().getHtmlItemsTable().rows.length;
			for(var _row = 0; _row < l && position == -1; _row++)
			{			
				_rootRow = getTable().getHtmlItemsTable().rows[_row]; 
				if(_rootRow.getAttribute("type") != "1" && _rootRow.getAttribute("type") != "2" && _rootRow.getAttribute("type") != "6" && _rootRow.getAttribute("type") != "7" && _rootRow.getAttribute("type") != "10") 
				{
					if(_rootRow == rootInnerRow && _rootRow.rowIndex == rootInnerRow.rowIndex) 
					{
						position = _indexPos; 
						if(getGridEX().getRootTable().getNewRowPosition() == 2) 
							position++; 
						if(getGridEX().getFilterMode() == 1)
							position++;
					}
					else
						_indexPos++; 
				}
			}
		}
	}	
	if(getTable().getRowHeaders())
		rowHeaderCell = getRowHeaderCell(); 
	var gridEXRow = this; 	
	return this; 
}
function GridEXTable(div, id, parent, rowpos, gridex)
{
	var id = id;
	var gridEX = null;		
	var parentTable = parent;
	if(parentTable == null) 
		gridEX = gridex;

	var rowheaders = false;
	var usecolheaders = false; 	
	var usecolumnsets = false;
	var cellLayoutMode = -1;	
	var isParentTable = false; 
	var headerWidth = -1;
	var headerType = -1;
	var hierarchicalMode = -1; 
	var key = null; 
	var filterRowHeight = 0;
	var newRecordHeight = 0;
	var tableHeaderHeight = 0;
	var pagerNavigatorHeight = 0; 	
	var selectorstatus = false; 
	var separatorHeight = 0; 
	var autosizeexpandcolumn = null; 
	var collapsedPreviewRowGlyph = ""; 
	var columnsCollection = null; 
	var expandedPreviewRowGlyph = ""; 	
	var hiddenColumnsCount = 0;
	var hiddenColumns = null;			
	var divtable = null; 
	if(parentTable == null)
		divtable = div;
	
	var tabCellsOrder = null; 
	var tableHeaders = null;
	var columnHeaders = null;
	var columnSets = null;
	var table = null;	
	var childTables = null;	
	var childTablesArray = null; 
	var rowsCss = null; 	
	var allowAddNew = false; 
	var allowDelete = false; 
	var allowEdit = false; 
	var newRowPosition = -1; 	
	var previewRow = false; 	
	this.getAllowAddNew = getAllowAddNew; 
	this.getAllowDelete = getAllowDelete; 
	this.getAllowEdit = getAllowEdit; 
	this.getAutoSizeExpandColumn = getAutoSizeExpandColumn;
	this.getChildTables = getChildTables; 
	this.getCollapsedPreviewRowGlyph = getCollapsedPreviewRowGlyph; 
	this.getColumns = getColumns;
	this.getExpandedPreviewRowGlyph = getExpandedPreviewRowGlyph;
	this.getGridEX = getGridEX;
	this.getGridEXColumnByClientID = getGridEXColumnByClientID; 
	this.getHeaders = getHeaders; 
	this.getHeaderWidth = getHeaderWidth; 
	this.getHiddenColumns = getHiddenColumns;
	this.getHierarchicalMode = getHierarchicalMode; 
	this.getHtmlDiv = getHtmlDiv;
	this.getHtmlItemsTable = getHtmlItemsTable; 
	this.getID = getID; 
	this.getIsParentTable = getIsParentTable; 
	this.getKey = getKey; 
	this.getNewRowPosition = getNewRowPosition; 
	this.getParent = getParent;
	this.getPreviewRow = getPreviewRow; 
	this.getRecordsCount = getRecordsCount; 
	this.getRowCss = getRowCss;
	this.getRowHeaders = getRowHeaders;	
	this.getSelectorStatus = getSelectorStatus; 	
	this.getUseColumnSets = getUseColumnSets; 	
	this.getWidth = getWidth; 
	this.setSelectorStatus = setSelectorStatus;
	this.AutoSizeColumn = AutoSizeColumn; 
	this.AutoSizeColumns = AutoSizeColumns;
	this.AutoSizeExpandColumn = AutoSizeExpandColumn; 		
	this.FindDisplayAndValueByText = FindDisplayAndValueByText; 
	this.FindRowByValue = FindRowByValue;
	this.FindRowWithDisplay = FindRowWithDisplay; 
	this.FixedResize = FixedResize; 
	this.HitTestColumnHeaders = HitTestColumnHeaders;
	this.IsParentOf = IsParentOf; 
	this.ResizeHeaderInRow = ResizeHeaderInRow; 	
	this.ResizeHeight = ResizeHeight;	
	setTableDefinition(getGridEX().getTableDefinition());	
	setRowsCss(); 	
	this.tabCellsOrder = tabCellsOrder;
	function getAllowAddNew() { return allowAddNew; }	
	function getAllowDelete() { return allowDelete; }	
	function getAllowEdit() { return allowEdit; }
	function getAutoSizeExpandColumn() { return autosizeexpandcolumn; }
	function getChildTables() { return childTables; }	
	function getGridEXColumnByClientID(columnID) { return getColumns().getGridEXColumnByClientID(columnID); }	
	function getCollapsedPreviewRowGlyph() { return collapsedPreviewRowGlyph; }	
	function getColumnSets() { return columnSets; }	
	function getExpandedPreviewRowGlyph() { return expandedPreviewRowGlyph; }	
	function getGridEX()
	{
		if(parentTable == null) 
			return gridEX;
		else 
			return parentTable.getGridEX(); 
	}
	function getHierarchicalMode() { return hierarchicalMode;  }
	function getIsParentTable() { return isParentTable; }	
	function getNewRowPosition() { return newRowPosition; }	
	function getParent() { return parentTable; }
	function getPreviewRow() { return previewRow; }
	function getRecordsCount()
	{	
		var recordscount = 0; 
		var row = null; 
		var itemrow = null; 
		var itemstable = getGridEX().getRootTable().getHtmlItemsTable(); 		
		var l = itemstable.rows.length; 
		var i = 0; 
		while(i<l)
		{			
			itemrow = itemstable.rows[i]; 
			if(itemrow.getAttribute("type") == null || (itemrow.getAttribute("type") == 3 || itemrow.getAttribute("type") == 4))
			{
				if(itemrow.getAttribute("t") == getID())
					recordscount++;
			}
			i++;
		}		
		return recordscount; 
	}
	function getHeaderWidth() { return (headerWidth == -1) ? 0 : headerWidth;  }	
	function getHeaders()
	{
		if(parentTable == null && !getGridEX().isHierarchicalGrid())
			return columnHeaders[0];
		else
			return columnHeaders; 			
	}	
	function getRowHeaders() { return rowheaders;}
	function getSelectorStatus() { return selectorstatus; }
	function setSelectorStatus(value) { selectorstatus = value;  }
	function getHtmlDiv()
	{
		if(parentTable != null)
			return parentTable.getHtmlDiv();
		else		
			return divtable; 
	}	
	function getHtmlItemsTable()
	{
		if(parentTable != null)
			return parentTable.getHtmlItemsTable(); 
		else
			return table; 
	}	
	function getColumns() { return columnsCollection; }	
	function getHiddenColumns(rowid)
	{
		if(hiddenColumns != null)
		{
			var index = 0;
			while(index < hiddenColumns.length)
			{
				if(hiddenColumns[index] == rowid)
				{
					var rowhiddencolumns = new Array(); 
					for(var icolumn = index + 1; icolumn < (index + 1) + (hiddenColumnsCount * 2); icolumn++)					
						rowhiddencolumns[rowhiddencolumns.length] = hiddenColumns[icolumn];
					return rowhiddencolumns; 					
				}
				index = index + (hiddenColumnsCount * 2) + 1; 
			}			
		}		
		return null; 		
	}	
	function getID() { return id; }	
	function getKey() { return key; }	
	function getUseColumnSets() { return (usecolumnsets == true || cellLayoutMode == 2); }	
	function getWidth()
	{
		if(usecolheaders)
		{	
			for(var i = 0; i < columnHeaders.length; i++)
			{
				if(columnHeaders[i].getIsVisible())
					return columnHeaders[i].getHtmlHeader().offsetWidth; 
			}
			throw new Error("invalid operation exception"); 
		}
		else
			return getFirstRecord().offsetWidth;
	}
	function getRowCss(rowType)
	{		
		if(rowsCss.length == 0)
			return ""; 
			
		var _length = rowsCss.length; 
		for(var index = 0; index < _length; index = index + 2)
		{
			if(rowsCss[index] == rowType)
				return rowsCss[index+1]; 
		}		
		return "";
	}		
	function getCellLayoutMode() { return cellLayoutMode; }
	var _firstRecord = null; 
	var _hierarchicalwidth = 0; 
	var cellsresize = 0; 
	function getFirstRecord()
	{
		if(_firstRecord == null)
		{
			if(getGridEX().getRootTable().getHtmlItemsTable().getAttribute("empty") != null)
				return null; 
				
			var scancomplete = false; 
			var _row = null; 
			var _index = 0; 
			var _rowslength = getHtmlItemsTable().rows.length; 
			while(_index < _rowslength && !scancomplete)
			{			
				_row = getHtmlItemsTable().rows[_index]; 
				if(_row.getAttribute("id") != null && _row.getAttribute("t") == getID() && (_row.getAttribute("type") == null || _row.getAttribute("type") == 3 || _row.getAttribute("type") == 4))
					scancomplete = true; 
		
				_index++; 
			}
			if(scancomplete)
			{
				_hierarchicalwidth = 0; 
				if(getGridEX().isHierarchicalGrid())
				{
					_row = _row.cells[0].childNodes[0].rows[0]; 
					for(var _hcell = 0; _hcell < _row.cells.length && _firstRecord == null; _hcell++)
					{
						if(_row.cells[_hcell].childNodes.length > 0 && _row.cells[_hcell].childNodes[0].nodeType == 1 && _row.cells[_hcell].childNodes[0].tagName == "TABLE")
						{							
							_firstRecord = _row.cells[_hcell].childNodes[0].rows[0]; 
							if(cellLayoutMode == 2)
								_firstRecord = _firstRecord.cells[0].childNodes[0].rows[0]; 
						}
						else
							_hierarchicalwidth += _row.cells[_hcell].offsetWidth; 
					}
				}
				else
				{
					_firstRecord = _row; 
					if(cellLayoutMode == 2)
						_firstRecord = _firstRecord.cells[0].childNodes[0].rows[0]; 
				}
			}			
			if(cellLayoutMode != 2)
			{
				if(_firstRecord == null)
					return; 
					
				var _cellslength = _firstRecord.cells.length; 
				for(var icell = 0; icell < _cellslength; icell++)
				{
					var cell = _firstRecord.cells[icell]; 
					if(cell.type != "rh")
					{
						if(getGridEXColumnByClientID(getColumnIDFromCellID(cell.id)).getAllowSize())
							cellsresize++; 				
					}
				}
			}
		}
		return _firstRecord; 
	}
	function getFixedTop()
	{
		var _top = 0; 
		_top += getGridEX().getPixelTop(); 
		_top += divtable.offsetTop; 
		if(getHeaders() != null)
		{
			if(getGridEX().isHierarchicalGrid())
				_top += getHeaders()[0].getHtmlHeader().offsetHeight; 
			else
				_top += getHeaders().getHtmlHeader().offsetHeight; 
		}			
		return _top;
	}	
	function getTableHeader()
	{
		if(parentTable == null)		
			return tableHeaders[0]; 
		else
			return tableHeaders;
	}		
	var _divRootHeader = -1; 
	var _divRootTableHeader = -1; 
	var _divRootNewRecord = -1; 	
	var _divRootFilterRow = -1;
	function getDivRoot(divType)
	{
		var _element = null; 
		var _divChildNodesLength = getHtmlDiv().childNodes.length; 
		for(var i = 0; i < _divChildNodesLength; i++)
		{
			_element = getHtmlDiv().childNodes[i]; 
			if(_element.tagName =="DIV" && _element.getAttribute("type") != null && parseInt(_element.getAttribute("type"), 10) == divType)
				return _element; 			
		}				
		return null;
	}	
	function table_onblur()
	{
		if(!getGridEX().getHtmlGridEX().contains(document.activeElement) || !getGridEX().getHtmlGridEX().contains(window.event.srcElement))
			getGridEX().gridEX_onblur(); 
	}
	var scrollStatus = null; 	
	function table_onscroll()
	{	
		raiseEventHandler(null,true); 
		var scrollLeft = table.offsetParent.scrollLeft;		
		if(scrollLeft  >= 0)
		{			
			if(_divRootHeader == -1)
				_divRootHeader = getDivRoot(1); 
			if(_divRootHeader != null)
				_divRootHeader.style.left = (scrollLeft * -1) +  "px";
			if(_divRootTableHeader == -1)			
				_divRootTableHeader = getDivRoot(2);
			if(_divRootTableHeader != null)			
				_divRootTableHeader.style.left = (scrollLeft * -1) + "px"; 
			if(_divRootNewRecord == -1)
				_divRootNewRecord = getDivRoot(7);
			if(_divRootNewRecord != null)
				_divRootNewRecord.style.left = (scrollLeft * -1) + "px"; 
			if(_divRootFilterRow == -1)
				_divRootFilterRow = getDivRoot(9); 
			if(_divRootFilterRow != null)
				_divRootFilterRow.style.left = (scrollLeft * -1) + "px"; 
		}
		if(scrollStatus == null)
			scrollStatus = document.getElementsByName(getGridEX().getID() + "_scrollstatus")[0];
		if(scrollStatus != null)
			scrollStatus.value = table.offsetParent.scrollTop;
		getGridEX().FireEvent("Scroll", [getGridEX()]); 
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
			drag_onmousemove(); 
			window.event.cancelBubble = true; 			
		}					
	}			
	function table_onselectstart() { }	
	var resyncWidth = false; 
	var _minimalSizes = null; 
	function getMinimalColumnSize(colid, colwidth)
	{
		if(_minimalSizes == null)
			_minimalSizes = new Array(); 		
		
		for(var i=0; i<_minimalSizes.length; i=i+2)
		{
			if(_minimalSizes[i] == colid)
			{
				if(resyncWidth && _minimalSizes[i + 1] != colwidth)
				{
					_minimalSizes[i+1] = colwidth; 
					resyncWidth = false; 
				}						
				return _minimalSizes[i+1]; 
			}				
		}			
		_minimalSizes[_minimalSizes.length] = colid;
		_minimalSizes[_minimalSizes.length] = colwidth;
		return colwidth;		
	}
	function AutoSizeExpandColumn(cell)
	{
		var columnid = getColumnIDFromCellID(cell.getAttribute("id")); 
		var gridEXColumn = getColumns().getGridEXColumnByClientID(columnid); 
		var maxColumnSize = getMaximumColumnSize(gridEXColumn); 
		if(maxColumnSize == -1)
			return; 
			
		if(cell.getAttribute("pec") != null)
			maxColumnSize += 18; 
			
		var _minimalColumnSize = getMinimalColumnSize(columnid, cell.offsetWidth); 
		if(maxColumnSize < _minimalColumnSize)
			maxColumnSize = _minimalColumnSize; 
			
		if(usecolheaders)
		{
			var column = columnHeaders[0].getHtmlColumnById(columnid); 
			columnHeaders[0].AutoSizeColumn(column, maxColumnSize); 
		}
		else
		{
			if(getCellLayoutMode() == 2)
			{
				//AutoSizeColumnSetCells(cell, maxColumnSize); 
			}
			else
				AutoSizeCells(cell, maxColumnSize); 
		}
	}
	function AutoSizeCells(column, width)
	{
		var _col = null; 
		if(!getGridEX().getColumnAutoResize())
		{
			var _itemsTables = null; 
			if(document.getChildsById != null)
				_itemsTables = document.getChildsById(getID() + "_i"); 
			else
				_itemsTables = allChildsCore(getID() + "_i",document.getElementsByTagName("*")); 
			var _itemsTablesLength = _itemsTables.length; 
			if(_itemsTablesLength > 0)
			{
				for(var i=0; i < _itemsTablesLength; i++)
				{
					_itemsCols = _itemsTables[i].getElementsByTagName("COL"); 
					_col = _itemsCols[column.cellIndex]; 
					_col.width = width + "px"; 
					updateTableSize(_itemsTables[i], _itemsCols); 		
				}				
			}
		}		
	}
	function AutoSizeByColumns() 
	{			
		if(usecolheaders && (headerType == 1 || headerType == 0))
		{
			var _columnHeaders = getHeaders();
			if(_columnHeaders != null)
			{
				if(parentTable == null && !getGridEX().isHierarchicalGrid())
					_columnHeaders.AutoSizeColumns(null); 
				else
					_columnHeaders[0].AutoSizeColumns(_columnHeaders); 
			}			
		}
		else
			AutoSizeByCells(); 
				
		if(childTables != null)
		{			
			for(var i = 0; i < childTables.Count(); i++)
				childTables.getTableInIndex(i).AutoSizeColumns(); 
		}
	}
	function AutoSizeColumn(column)
	{
		if(usecolheaders)
			columnHeaders[0].ColumnAutoSize(column, null); 
	}
	function AutoSizeColumns() { AutoSizeByColumns(); }
	function AutoSizeByCells()
	{		
		getFirstRecord();	
		if(_firstRecord == null)
		{	
			var t = getGridEX().getRootTable().getHtmlItemsTable(); 
			if(t.getAttribute("empty") != null)
			{				
				var cs = t.getElementsByTagName("COL"); 
				for(var i=0;i<cs.length;i++)
					cs[i].width = (getGridEX().getResizeWidth() / cs.length) + "px";				
					
				t.style.width = getGridEX().getResizeWidth() + "px"; 
			}				
			return; 
		}	
		if(cellLayoutMode == 2) 
		{
			var _tmpcell = null; 
			var _cellslength = -1; 
			var _columnsetcellsLength = _firstRecord.cells.length; 
			var _columnsettable = null; 
			var _cols = null; 
			var cell = null; 						
			var _oldwidth = _firstRecord.offsetWidth; 
			var offset = 0;
			var originalsets = new Array(); 
			var _fixedcolumnsets = null; 
			for(var icolumnset = 0; icolumnset < _columnsetcellsLength; icolumnset++)
			{
				_tmpcell = _firstRecord.cells[icolumnset];
				if(_tmpcell.getAttribute("type") != "rh")
				{					
					originalsets[originalsets.length] = _tmpcell.offsetWidth; 
					_columnsettable = _tmpcell.childNodes[0]; 
					if(_columnsettable.cells != null)
						_cellslength = _columnsettable.cells.length; 
					else
						_cellslength = getCellsCore(_columnsettable).length; 
					var _fixedwidth = new Array(_columnsettable.getElementsByTagName("COL").length);
					for(var _icell = 0; _icell < _cellslength; _icell++)
					{					
						if(_columnsettable.cells != null)
							cell = _columnsettable.cells[_icell];
						else
							cell = getCellsCore(_columnsettable)[_icell];
						if(cell.getAttribute("id") != null && cell.getAttribute("id") != "")
						{
							_colid = getColumnIDFromCellID(cell.id); 
							if(_colid != "" && !getGridEXColumnByClientID(_colid).getAllowSize())							
								_fixedwidth[cell.getAttribute("usecol")] = cell.offsetWidth; 																						
						}						
					}
					var _tmpwidth = 0; 
					for(var _ifixedwidth = 0; _ifixedwidth < _fixedwidth.length; _ifixedwidth++)
					{
						if(_fixedwidth[_ifixedwidth] != null)
							_tmpwidth += _fixedwidth[_ifixedwidth]; 
					}
					if(_fixedcolumnsets == null)
						_fixedcolumnsets = new Array(); 
					_fixedcolumnsets[_fixedcolumnsets.length] = _tmpwidth; 
				}				
			}
			var _newwidth = getGridEX().getResizeWidth() - (getHeaderWidth() + _hierarchicalwidth); 
			_oldwidth -= getHeaderWidth(); 
			var _gridexcolumn = null; 
			var _newcolumnsetsize = null;
			var _newcolumnsets = new Array(); 
			var newcolumnsetwidth = -1; 
			var actualwidth = 0; 
			for(var icolumnset = 0; icolumnset < _columnsetcellsLength; icolumnset++)
			{
				_tmpcell = _firstRecord.cells[icolumnset]; 
				if(_tmpcell.getAttribute("type") != "rh")
				{	
					newcolumnsetwidth = Math.round((originalsets[icolumnset] * _newwidth) / _oldwidth); 
					_columnsettable = _tmpcell.childNodes[0]; 
					if(_columnsettable.cells != null)
						_cellslength = _columnsettable.cells.length; 
					else
						_cellslength = getCellsCore(_columnsettable).length;
					_cols = _columnsettable.getElementsByTagName("COL"); 
					var _newcellswidth = new Array(_cols.length); 
					for(var _icell = 0; _icell < _cellslength; _icell++)
					{
						if(_columnsettable.cells != null)
							cell = _columnsettable.cells[_icell]; 
						else
							cell = getCellsCore(_columnsettable)[_icell];
						if(cell.getAttribute("id") != null && cell.getAttribute("id") != "")
						{
							_colid = getColumnIDFromCellID(cell.getAttribute("id")); 
							_gridexcolumn = getGridEXColumnByClientID(_colid); 
							if(_gridexcolumn.getAllowSize())
							{							
								_newcellwidth = Math.round((getPixelColWidth(_cols[_gridexcolumn.getColumnSetColumn()].width) * (newcolumnsetwidth - _fixedcolumnsets[icolumnset])) / (originalsets[icolumnset] - _fixedcolumnsets[icolumnset])); 
								_newcellswidth[_gridexcolumn.getColumnSetColumn()] = _newcellwidth;
							}
						}
					}
					for(var _icell = 0; _icell < _newcellswidth.length; _icell++)
					{
						if(_newcellswidth[_icell] != null)
							actualwidth += _newcellswidth[_icell]; 
					}
					_newcolumnsets[_newcolumnsets.length] = _newcellswidth; 
				}
			}
			var diffsize = _newwidth - actualwidth; 
			if(diffsize != 0)
			{
				if(diffsize < 0)
					offset = -1; 
				else if(diffsize > 0)
					offset = 1;
										
				do
				{				
					for(var _icolumnset = 0; _icolumnset < _newcolumnsets.length && diffsize != 0; _icolumnset++)
					{
						_newcolumnsetsize = _newcolumnsets[_icolumnset];
						for(var _icol = 0; _icol < _newcolumnsetsize.length && diffsize != 0; _icol++)
						{
							if(_newcolumnsetsize[_icol] + offset > 0)
							{
								_newcolumnsetsize[_icol] += offset; 
								actualwidth += offset; 
								diffsize = _newwidth - actualwidth; 
							}
						}
					}
				} while(diffsize != 0); 
			}	
			var _itemsCols = null;
			var _itemsTables = null; 
			var _itemsTablesLength = -1; 	
			for(var icolumnset = 0; icolumnset < _newcolumnsets.length; icolumnset++)
			{
				if(document.getChildsById != null)				
					 _itemsTables = document.getChildsById(getID() + "_items_cs" + icolumnset); 
				else
					_itemsTables = allChildsCore(getID() + "_items_cs" + icolumnset, document.getElementsByTagName("*")); 
											
				 _itemsTablesLength = _itemsTables.length; 
				 for(var _item = 0; _item < _itemsTablesLength; _item++)
				 {
					_itemsCols = _itemsTables[_item].getElementsByTagName("COL"); 
					_newcolumnsetsize = _newcolumnsets[icolumnset]; 
					for(var _icol = 0; _icol < _newcolumnsetsize.length; _icol++)
					{
						if(_newcolumnsetsize[_icol] != null && (_newcolumnsetsize[_icol] != getPixelColWidth(_itemsCols[_icol].width)))
							_itemsCols[_icol].width = _newcolumnsetsize[_icol] + "px"; 
					}
				 }				
			}
		}
		else
		{
			var newwidth = getGridEX().getResizeWidth(); 
			var cellswidth = new Array();
			var _fixedwidth = 0; 			
			var _col = null; 
			var _colid = ""; 
			var _cellwidth = -1; 
			var _cellsLength = _firstRecord.cells.length; 
			var _oldwidth = _firstRecord.offsetWidth; 
			var igcell = 0; 
			for(var icell = 0; icell < _cellsLength; icell++)
			{
				cell = _firstRecord.cells[icell];
				if(cell.getAttribute("type") != "rh")
				{
					_colid = getColumnIDFromCellID(cell.getAttribute("id"));
					if(_colid != "" && getGridEXColumnByClientID(_colid).getAllowSize())
						cellswidth[cellswidth.length] = cell.offsetWidth; 
					else
						_fixedwidth += cell.offsetWidth;
				}
			}
			_oldwidth = _oldwidth - _fixedwidth; 
			newwidth = newwidth - (_fixedwidth + _hierarchicalwidth);
			var actualwidth = 0; 
			var newsizes = new Array(); 
			for(var icell = 0; icell < _cellsLength; icell++)
			{
				cell = _firstRecord.cells[icell]; 
				if(cell.getAttribute("type") != "rh")
				{
					_colid = getColumnIDFromCellID(cell.getAttribute("id")); 
					if(_colid != "" && getGridEXColumnByClientID(_colid).getAllowSize())
					{
						_cellwidth = Math.round((cellswidth[igcell] * newwidth) / _oldwidth); 
						newsizes[newsizes.length] = cell.cellIndex; 
						newsizes[newsizes.length] = _cellwidth; 
						actualwidth += _cellwidth; 
						igcell++;						
					}
				}
			}			
			var countzero = 0; 
			var diffsize = newwidth - actualwidth;
			var newsizeslength = newsizes.length; 
			var offset = 0; 
			if(diffsize != 0)
			{			
				if(diffsize < 0)
					offset = -1; 
				else if(diffsize > 0)
					offset = 1;						
				do
				{
					for(var index = 0; index < newsizeslength && (diffsize != 0 && countzero != cellsresize); index = index + 2)
					{
						if(newsizes[index+1] + offset > 0)
							newsizes[index+1] += offset; 
						else
							countzero++; 
							
						actualwidth += offset;
						diffsize = newwidth - actualwidth; 
					}
				} while(diffsize != 0 && countzero != cellsresize);
			}
			var _itemsTables = null; 
			if(document.getChildsById != null)
				_itemsTables = document.getChildsById(getID() + "_i"); 
			else
				_itemsTables = allChildsCore(getID()+"_i", document.getElementsByTagName("*")); 
			var _itemsTablesLength = _itemsTables.length; 
			if(_itemsTablesLength > 0)
			{
				for(var _item = 0; _item < _itemsTablesLength; _item++)
				{
					_itemsCols = _itemsTables[_item].getElementsByTagName("COL"); 
					for(var _index = 0; _index < newsizeslength; _index = _index + 2)
					{
						_col = _itemsCols[newsizes[_index]]; 
						_col.width = newsizes[_index+1] + "px"; 
					}
				}				
			}
		}
	}
	function FixedResize()
	{
		if(getParent() != null)
			throw Error("invalid method operation call"); 
			
		FixedResizeWidth(); 				
		ResizeHeight(); 
	}
	function IsParentOf(childID)
	{
		if(childTablesArray == null)
			return false; 
			
		for(var i = 0; i < childTablesArray.length; i++)
		{
			if(childTablesArray[i] == childID)
				return true; 
		}
		return false; 
	}
	function ResizeHeaderInRow(rowIndex, resizeRows)
	{		
		if(columnHeaders != null)
		{
			var _columnHeader = null; 
			var _columnHeadersLength = columnHeaders.length; 
			for(var i = 0; i <  _columnHeadersLength; i++)
			{
				_columnHeader = columnHeaders[i]; 
				if(_columnHeader.getRowIndex() == rowIndex)
				{
					_columnHeader.AutoSizeColumnsAfterDisplay(); 
					return;
				}
			}
		}
	}	
	var percentFactor = -1; 
	function ResizeHeight()
	{	
		if(getParent() == null)
		{	
			var divgridex = getGridEX().getHtmlGridEX();
			var gridexparent = getGridEXOffsetParent(divgridex); 
			if(gridexparent != null)
			{			
				var clientheight = -1;
				if(gridexparent.tagName == "BODY")
					clientheight = window.innerHeight;
				else
					clientheight = gridexparent.offsetHeight;
					
				var offset = getBorderStyleWidth(divgridex.style.borderTopWidth) + getBorderStyleWidth(divgridex.style.borderBottomWidth);
				if(isDefaultView)				
					offset += getPixelValue(document.defaultView.getComputedStyle(divgridex, null).getPropertyValue("top"));
				else
					offset += getPixelValue(divgridex.style.top); 
				offset += getBottomOffset(gridexparent) + getTopOffset(gridexparent); 									
				if(percentFactor == -1 && (divgridex.style.height != null && divgridex.style.height.indexOf("%") >= 0))
					percentFactor = getPercentWidth(divgridex.style.height) / 100;
					
				if(percentFactor != -1)
					clientheight = clientheight * percentFactor;
									
				clientheight = clientheight - offset; 												
				var obj = null;				
				obj = getGridEX().getGroupByBox();
				if(obj != null) 
					clientheight -= obj.getHeight(); 
				
				obj = getTableHeader(); 
				if(obj != null && obj.length > 0) 
					clientheight -= obj[0].offsetHeight;
					
				obj = getHeaders(); 
				if(obj != null && !getGridEX().isHierarchicalGrid())
					clientheight -= obj.getHtmlHeader().offsetHeight; 
				else if(obj != null && obj.length > 0 && getGridEX().isHierarchicalGrid())
					clientheight -= obj[0].getHtmlHeader().offsetHeight;
					
				clientheight -= newRecordHeight;
				clientheight -= filterRowHeight; 
				clientheight -= separatorHeight; 
				clientheight -= pagerNavigatorHeight;		
				clientheight -= tableHeaderHeight;				
				if(clientheight <= 0)				
					return;
					
				table.offsetParent.style.height = (clientheight) + "px";
			}
		}
	}	
	function FixedResizeWidth() { }	
	function FindRowByValue(value)
	{	
		var _rows = getHtmlItemsTable().rows;		
		var _innerCellsLength = null; 
		var _innerRow = null; 		
		var _isHierarchicalGrid = getGridEX().isHierarchicalGrid(); 
		var _isColumnSet = (getCellLayoutMode() == 0); 
		if(_rows.length > 0)
		{			
			var row = 0; 
			var rowsLength = _rows.length;
			var _tmpRow = null; 
			var _tmpTable = null; 
			while(row < rowsLength)
			{				
				_tmpRow = _rows[row];
				if(_tmpRow.getAttribute("type") == null || (parseInt(_tmpRow.getAttribute("type"), 10) == 3 || parseInt(_tmpRow.getAttribute("type"), 10) == 4))
				{					
					if(_isHierarchicalGrid)
					{					
						if(_isColumnSet)
						{
						}
						else
						{
							_tmpTable = _tmpRow.cells[0].childNodes[0];
							_innerRow = _tmpTable.rows[0]; 
							_innerCellsLength = _innerRow.cells.length;
							for(var _cell = 0; _cell < _innerCellsLength; _cell++)
							{
								if(_innerRow.cells[_cell].childNodes[0].tagName == "TABLE")
								{	
									if(_innerRow.cells[_cell].childNodes[0].rows[0].value != null && _innerRow.cells[_cell].childNodes[0].rows[0].value == value)
										return [_tmpRow, _innerRow.cells[_cell].childNodes[0].rows[0]];
								}
							}
						}						
					}
					else
					{					
						if(_isColumnSet) 
						{							
						}
						else
						{
							if(_tmpRow.getAttribute("value") != null && _tmpRow.getAttribute("value") == value)
								return [_tmpRow,_tmpRow]; 
						}
					}
				}
				row++; 
			}			
		}
		return null; 
	}	
	function FindRowWithDisplay(text)
	{
		if(text == null || text.length == 0)
			return null;
			
		var _rows = getHtmlItemsTable().rows;		
		var _isHierarchicalGrid = getGridEX().isHierarchicalGrid(); 
		var _isColumnSet = (getCellLayoutMode() == 0);				
		var _display = null; 
		var _indexOf = -1; 
		var _innerCellsLength = -1; 
		var _innerRow = null; 
		var _tmpRow = null; 
		var _tmpTable = null; 		
		var row = -1;
		var rowsLength = -1;		
		text = trim(text); 		
		if(_rows.length > 0)
		{					
			row = 0; 
			rowsLength = _rows.length;			
			while(row < rowsLength)
			{	
				_indexOf = -1; 
				_tmpRow = _rows[row];
				if(_tmpRow.getAttribute("type") == null || (parseInt(_tmpRow.getAttribute("type"), 10) == 3 || parseInt(_tmpRow.getAttribute("type"), 10) == 4))
				{					
					if(_isHierarchicalGrid)
					{					
						if(_isColumnSet)
						{
						}
						else
						{
							_tmpTable = _tmpRow.cells[0].childNodes[0];
							_innerRow = _tmpTable.rows[0]; 
							_innerCellsLength = _innerRow.cells.length;
							for(var _cell = 0; _cell < _innerCellsLength; _cell++)
							{
								if(_innerRow.cells[_cell].childNodes[0].tagName == "TABLE")
								{										
									_display = _innerRow.cells[_cell].childNodes[0].rows[0].getAttribute("displayMember"); 									
									if(_display != null)
									{
										_display = trim(_display); 										
										_indexOf = _display.toUpperCase().indexOf(text.toUpperCase()); 
										if(_indexOf == 0)
											return [_tmpRow.getAttribute("id"), _innerRow.cells[_cell].childNodes[0].rows[0]];
									}
								}
							}
						}						
					}
					else
					{					
						if(_isColumnSet) 
						{							
						}
						else
						{							
							_display = _tmpRow.getAttribute("displayMember");
							if(_display != null)
							{
								_display = trim(_display); 
								_indexOf = _display.toUpperCase().indexOf(text.toUpperCase());
								if(_indexOf == 0)
									return [_tmpRow.getAttribute("id"), _tmpRow]; 
							}
						}
					}
				}
				row++; 
			}
		}		
		return null; 
	}	
	function FindDisplayAndValueByText(value)
	{
		if(value == null || value.length == 0)
			return null; 
			
		value = trim(value); 	
		var _display = null; 
		var _value = null;		
		var _rows = getHtmlItemsTable().rows;		
		var _indexOf = -1; 
		var _innerCellsLength = null;
		var _innerRow = null;		
		var _isHierarchicalGrid = getGridEX().isHierarchicalGrid(); 
		var _isColumnSet = (getCellLayoutMode() == 0);		
		if(_rows.length > 1)
		{			
			var row = 0; 
			var rowsLength = _rows.length;
			var _tmpRow = null; 
			var _tmpTable = null; 
			while(row < rowsLength)
			{				
				_display = null;
				_indexOf = -1; 
				_tmpRow = _rows[row];
				if(_tmpRow.getAttribute("type") == null || (parseInt(_tmpRow.getAttribute("type"), 10) == 3 || parseInt(_tmpRow.getAttribute("type"), 10) == 4))
				{					
					if(_isHierarchicalGrid)
					{					
						if(_isColumnSet)
						{
						}
						else
						{
							_tmpTable = _tmpRow.cells[0].childNodes[0];
							_innerRow = _tmpTable.rows[0]; 
							_innerCellsLength = _innerRow.cells.length;
							for(var _cell = 0; _cell < _innerCellsLength; _cell++)
							{
								if(_innerRow.cells[_cell].childNodes[0].tagName == "TABLE")
								{	
									_value = _innerRow.cells[_cell].childNodes[0].rows[0].getAttribute("value");
									_display = _innerRow.cells[_cell].childNodes[0].rows[0].getAttribute("displayMember"); 									
									if(_display != null)
									{
										_display = trim(_display); 										
										_indexOf = _display.toUpperCase().indexOf(value.toUpperCase()); 
										if(_indexOf >= 0)
											return [_indexOf, _display, _value]; 
									}
								}
							}
						}						
					}
					else
					{					
						if(_isColumnSet) 
						{							
						}
						else
						{
							_value = _tmpRow.getAttribute("value"); 
							_display = _tmpRow.getAttribute("displayMember");
							if(_display != null)
							{
								_display = trim(_display); 
								_indexOf = _display.toUpperCase().indexOf(value.toUpperCase());
								if(_indexOf >= 0)
									return [_indexOf, _display, _value]; 
							}
						}
					}
				}
				row++; 
			}			
		}
		return null;
	}	
	function HitTestColumnHeaders(x, y)
	{
		var r = null; 
		var l = columnHeaders.length; 
		for(var i = 0; i < l; i++)
		{
			r = columnHeaders[i].HitTestColumns(x, y);
			if(r != null)
				return r; 
		}		
		return null; 
	}		
	function setTableDefinition(definitions)
	{	
		var DEFINITION_LENGTH = 20;	
		for(var index = 0; index < definitions.length; index += DEFINITION_LENGTH)
		{
			if(definitions[index] == id)
			{	
				key = definitions[index+1]; 
				hierarchicalMode = definitions[index+2];
				isParentTable = (definitions[index+3] == 1) ? true : false;
				childTablesArray = (definitions[index+4] != null && definitions[index+4].length > 0) ? definitions[index+4].split("$") : null;
				rowheaders = (definitions[index+5] == 1) ? true : false;
				usecolheaders = (definitions[index+6] == 1) ? true : false;
				cellLayoutMode = definitions[index+7];
				headerType = definitions[index+8];
				headerWidth = definitions[index+9]; 
				allowAddNew = (definitions[index+10] == 1) ? true : false;
				newRowPosition = definitions[index+11]; 
				allowEdit = (definitions[index+12] == 1) ? true : false;
				allowDelete = (definitions[index+13] == 1) ?  true : false; 
				previewRow = (definitions[index+14] == 1) ? true : false;
				expandedPreviewRowGlyph = definitions[index+15]; 
				collapsedPreviewRowGlyph = definitions[index+16];
				selectorstatus = (definitions[index+17] == 1) ? true : false; 
				autosizeexpandcolumn = (definitions[index+18] != null && definitions[index+18].length > 0) ? definitions[index+18] : null;
				tabCellsOrder = definitions[i+19];
				return; 
			}
		}
	}	
	function setRowsCss() { rowsCss = eval(getID() + "_client_rows_css"); }	
	function getTableColInPosition(position)      
	{
		var cols = table.all.tags("COL");
		for(var i=0; i< cols.length; i++)
		{
			if(cols[i].getAttribute("pos") != null)
			{
				if(parseInt(cols[i].getAttribute("pos"), 10) == position)
					return cols[i]; 
			}
		}
	}	
	function swapColumns(columnX, columnY)	
	{	
		var colx, coly; 
		var position;		
		colx = getTableColInPosition(columnX.getPosition());
		position = colx.getAttribute("pos");
		coly = getTableColInPosition(columnY.getPosition());
		colx.swapNode(coly);
		colx.getAttribute("pos") = coly.getAttribute("pos"); 
		coly.getAttribute("pos") = position;		
		var cellx = columnX.getHtmlColumn().cellIndex; 
		var celly = columnY.getHtmlColumn().cellIndex; 	
		columnX.getHtmlColumn().swapNode(columnY.getHtmlColumn()); 
		position = columnX.getPosition();
		columnX.setPosition(columnY.getPosition());
		columnY.setPosition(position);		 
		 var row = null; 
		for(var index = 0; index < table.rows.length; index++)
		{
			row = table.rows[index];
			if(row.getAttribute("type") != "recordPreview")
			{
				var cellid = row.cells[celly].childNodes[0].id; 			
				row.cells[celly].childNodes[0].id = row.cells[cellx].childNodes[0].id;
				row.cells[cellx].childNodes[0].id = cellid; 
				row.cells[cellx].swapNode(row.cells[celly]); 
			} 
		}
	}	
	function recordsTable(htmltable)
	{		
		if(htmltable != null)
		{
			var divs = htmltable.getElementsByTagName("DIV");			
			if(divs != null && divs.length > 0)
			{
				var _div = null; 
				var _divLength = divs.length; 
				for(var idiv = 0; idiv < _divLength; idiv++)
				{					
					_div = divs[idiv]; 
					if(_div.getAttribute("type") != null && parseInt(_div.getAttribute("type"), 10) == 4)
						return _div.getElementsByTagName("TABLE")[0];
				}
			}
		}
		return null;
	}	
	function setChildTables(gridEXTable, rowPos)
	{
		if(childTables != null && childTables.Count() == childTablesArray.length)
			return;
			
		var _table = getHtmlItemsTable();
		if(_table != null)
		{
			var _rows = _table.rows;
			var _row = null; 
			var _lastid = null; 
			for(var _index = rowPos; _index < _rows.length; _index++)
			{	
				if(childTables != null && childTables.Count() == childTablesArray.length)
					return; 
					
				_row = _rows[_index];
				if(_row.getAttribute("pt") != null &&  _row.getAttribute("pt") == id && _row.getAttribute("t") != _lastid)
				{													
					if(childTables == null)
						childTables = new GridEXChildTableCollection(gridEXTable);							
					if(childTables.getTableByID(_row.getAttribute("t")) == null)
					{
						childTables.Add(new GridEXTable(null, _row.getAttribute("t"), gridEXTable, _row.rowIndex, null));
						_lastid = _row.getAttribute("t");		
					}
				}
			}
		}
	}		
	function setColumnSetsFromTable(gridEXTable, htmltable) {	}	
	function getColumnSetsFromHeader(gridEXTable, gridEXHeader, divheader)
	{			
		if(divheader != null && divheader.children.length > 0)
		{			
			var htmlheader = divheader.childNodes[1]; 
			if(htmlheader != null)
			{					
				var columnsets = new GridEXColumnSetCollection(gridEXTable, htmlheader, true, gridEXHeader); 
				var row = htmlheader.rows[0];						
				var _cellsLength = row.cells.length; 
				var cell = null;
				var columnset = null; 
				for(var icell = 0; icell < _cellsLength; icell++)
				{
					cell = row.cells[icell]; 
					if(cell.all.tags("TABLE").length > 0)
					{						
						columnset = new GridEXColumnSet(gridEXTable, columnsets.getCount(), cell.all.tags("TABLE")[0], true, gridEXHeader);
						columnsets.Add(columnset);
					}
				}								
				return columnsets;
			}
		}		
		return null; 
	}	
	function getRootColumnHeader(gridEXTable, htmldiv, headerType, columnHeaders)
	{
		if(htmldiv != null)
		{
			var divs = htmldiv.getElementsByTagName("DIV");
			var _divsLength = divs.length; 
			if(divs != null && _divsLength > 0)
			{
				var _div = null; 				
				var columnsheader = null; 
				for(var index = 0; index <  _divsLength; index++)
				{	
					_div = divs[index]; 
					if(_div.getAttribute("type") != null && parseInt(_div.getAttribute("type"), 10) == 1)
					{
						columnsheader = new GridEXColumnHeaders(gridEXTable, null, _div.getElementsByTagName("TABLE")[0], headerType, 0, true); 
						if(columnHeaders == null)
							columnHeaders = new Array();
						columnHeaders[columnHeaders.length] = columnsheader;						
						return; 
					}
				}
			}
		}		
	}			
	function getChildrenColumnHeader(gridEXTable, rowPos, rootTable, headerType, columnHeaders)
	{		
		var columnsheader = null; 
		var _pos = columnHeaders.length; 
		var _length = rootTable.rows.length; 
		var _row = null; 
		for(var index = rowPos; index < _length; index++)
		{
			_row = rootTable.rows[index]; 
			if(_row.getAttribute("type") == "1" && _row.getAttribute("t") != null && _row.getAttribute("t") == gridEXTable.getID())
			{
				var _tmpDivs = _row.cells[0].getElementsByTagName("DIV")
				var _headerDiv = null; 
				for(var _iheaderDiv = 0; _iheaderDiv < _tmpDivs.length && _headerDiv == null; _iheaderDiv++)
				{
					if(_tmpDivs[_iheaderDiv].getElementsByTagName("TABLE").length > 0)
						_headerDiv = _tmpDivs[_iheaderDiv]; 
				}				
				if(_headerDiv == null)
					throw Error("unable to find DIV child header"); 			
				
				columnsheader = new GridEXColumnHeaders(gridEXTable, _row, _headerDiv.getElementsByTagName("TABLE")[0] , headerType, _pos, false); 
				if(columnHeaders == null)
					columnHeaders = new Array(); 
					
				columnHeaders[columnHeaders.length] = columnsheader;
				_pos++;
			}
		}
	}	
	function getRootTableHeader(htmltable, tableHeaders)
	{
		if(htmltable != null)
		{
			var divs = null; 
			if(htmltable.all == null)			
				divs = htmltable.getElementsByTagName("DIV"); 
			else
				divs = htmltable.all.tags("DIV");	
				
			if(divs != null && divs.length > 0)
			{
				var _div = null; 
				var l = divs.length; 
				for(var i = 0; i < l; i++)
				{
					_div = divs[i]; 
					if(_div.getAttribute("type") != null && parseInt(_div.getAttribute("type"), 10) == 2)
					{
						if(tableHeaders == null)
							tableHeaders = new Array(); 
							
						if(_div.all == null)
							tableHeaders[tableHeaders.length] = _div.getElementsByTagName("TABLE")[0];
						else
							tableHeaders[tableHeaders.length] = _div.all.tags("TABLE")[0]; 						
						return; 
					}
				}
			}			
		}
	}	
	function getChildrenTableHeader(gridEXTable, rowPos, rootTable, tableHeaders)
	{	
		var l = rootTable.rows.length; 
		var _row = null;
		for(var i = rowPos; i < l; i++)
		{
			_row = rootTable.rows[i];
			if(_row.getAttribute("pt") != null && _row.getAttribute("pt") == gridEXTable.getID() && _row.getAttribute("type") != null && parseInt(_row.getAttribute("type"), 10) == 2)
			{
				if(tableHeaders == null)
					tableHeaders = new Array(); 
					
				tableHeaders[tableHeaders.length] = _row.cells[0].all.tags("DIV")[0].all.tags("TABLE")[0]; 
			}
		}
	}
	if(parentTable == null && !getGridEX().isHierarchicalGrid())
	{		
		columnHeaders = new Array(); 
		tableHeaders = new Array(); 
		getRootTableHeader(divtable, tableHeaders); 		
		if(usecolheaders)
			getRootColumnHeader(this, divtable, headerType, columnHeaders);
		table = recordsTable(divtable);		
		if(headerType == 0 && columnHeaders != null && columnHeaders.length > 0)
		{
			var _columnSets = getColumnSetsFromHeader(this, columnHeaders[0], columnHeaders[0].getHtmlHeader()); // columnHeaders.getHtmlHeader()							
			columnHeaders[0].setColumnSets(_columnSets);
			usecolumnsets = (_columnSets != null) ? true : false; 
		}
		else if(headerType == 1 && cellLayoutMode == 2) 
			columnSets = setColumnSetsFromTable(this, table);
	}
	else if(parentTable == null && getGridEX().isHierarchicalGrid())
	{
		columnHeaders = new Array(); 
		tableHeaders = new Array(); 
		table = recordsTable(divtable);
		if(usecolheaders)
		{
			getRootColumnHeader(this, divtable, headerType, columnHeaders); 	
			getChildrenColumnHeader(this, 0, getHtmlItemsTable(), headerType, columnHeaders);  // rowpos						
		}
		if(headerType == 0 && columnHeaders != null && columnHeaders.length > 0)
		{
			var _columnHeader = null; 
			var _columnSets =null; 
			for(var i = 0; i < columnHeaders.length; i++)
			{
				_columnHeader = columnHeaders[i]; 
				_columnSets = getColumnSetsFromHeader(this, _columnHeader, _columnHeader.getHtmlHeader());
				usecolumnsets = (_columnSets != null) ? true : false; 
				_columnHeader.setColumnSets(_columnSets); 
			}
		}				
	}
	else
	{
		columnHeaders = new Array();
		tableHeaders = new Array(); 
		getChildrenTableHeader(this, rowpos, getHtmlItemsTable(), tableHeaders); 
		if(usecolheaders)
			getChildrenColumnHeader(this, rowpos, getHtmlItemsTable(), headerType, columnHeaders); 
	}	
	if(getGridEX().isHierarchicalGrid())
		setChildTables(this, rowpos);
		
	if(parentTable == null && divtable != null)
	{
		if(getGridEX().getHtmlGridEX().getElementsByTagName("DIV").length > 0)
		{			
			var _pagerdiv = getGridEX().getHtmlGridEX().getElementsByTagName("DIV")[0];
			if(parseInt(_pagerdiv.getAttribute("type"), 10) == 6)
				pagerNavigatorHeight += _pagerdiv.offsetHeight; 
		}
		var divs = divtable.getElementsByTagName("DIV");
		var _div = null;
		var l = divs.length; 
		for(var i = 0; i < l; i++)
		{
			_div = divs[i];
			if(parseInt(_div.getAttribute("type"), 10) == 6)
				pagerNavigatorHeight += _div.offsetHeight;
			else if(parseInt(_div.getAttribute("type"), 10) == 7) 
				newRecordHeight += _div.offsetHeight; 
			else if(parseInt(_div.getAttribute("type"), 10) == 8 && separatorHeight == 0)
				separatorHeight += _div.offsetHeight;
			else if(parseInt(_div.getAttribute("type"), 10) == 9)
				filterRowHeight += _div.offsetHeight;
			else if(parseInt(_div.getAttribute("type"), 10) == 2 && _div.parentElement == divtable)
				tableHeaderHeight += _div.offsetHeight;
		}
	}			
	if(table != null)
	{
		table.addEventListener("mousemove", table_onmousemove, false);
		table.addEventListener("select", table_onselectstart, false); 
	}		
	if(table != null)
	{		
		if(table.offsetParent != null)
		{			
			table.offsetParent.addEventListener("blur", table_onblur, false); 
			table.offsetParent.addEventListener("scroll", table_onscroll, false);
		}
		else
		{
			table.parentElement.addEventListener("blur", table_onblur, false); 
			table.parentElement.addEventListener("scroll", table_onscroll, false);
		}		
	}		
	columnsCollection = new GridEXColumnCollection(eval(getID() + "_client_columns"), this);
	var _gridEXTable = this;
	return this; 
}
function GridEX(id, clientDefinition, tablesDefinition, selectedItems, rowsCss, hiddenValues, clientEvents, fixTableSize)
{
	var id = id;	
	var clientEventsCount = -1; 
	var columnAutoResize = false;	
	var columnSetNavigation = 1; 
	var fixTableSize = fixTableSize; 
	var formID = ""; 
	var initialized = false; 
	var hierarchicalGrid = false; 		
	var hitArea = -1; 
	var selectionMode = 1;		
	var useHiddenColumns = false; 		
	var owner = null; 	
	var hiddenValuesCollection = hiddenValues; 
	var selectedItemsCollection = null;	
	var selectonexpand = false; 
	var tableDefinition = tablesDefinition;	
	var arrRowsCss = rowsCss;	
	var controlsToBuild = null; 
	var editControls = null;
	var replaceValues = false; 
	var resetFilterCommand = null; 	
	var htmlGridEX = document.getElementById(id);  
	if(htmlGridEX == null)
		throw Error("DIV object with ID '" + id + "' is null or invalid"); 
		
	if(htmlGridEX.getAttribute("name") == null || htmlGridEX.getAttribute("name") == "")
		throw Error("unable to find server id attribute on DIV");
		
	this.focusCss = "";
	this.focusRowCss = "";  
	this.currentCell = null;
	this.ddbimg = ""; 
	this.ddbpimg = ""; 
	this.selpb = false; 
	this.ddpb = false; 
	this.cmpb = false; 
	var isfiltered = false; 
	var htmlGridEXParent = null; 		
	var serverid = htmlGridEX.getAttribute("name");			
	var childLoadingMode = 1;
	var currentEditAction = -1; 
	var currentEditControl = null;	
	var filterMode = -1; 
	var haltEdition = false;	
	var gridEXRow = null;
	var gridEXRows = null;
	var initRowID = null; 		
	var resizeGroups = false; 
	var resizeHeight = false;
	var resizeMode = -1; 
	var resizeWidth = false; 
	var recordcount = 0; 
	var rowcount = 0; 
	var tables = null;	
	var groupByBox = null;
	var rootTable = null;			
	var eventhandlers = clientEvents;	
	var clientWidth = -1;	
	var isdropdown = false;	
	var updateMode = 1;	
	var updateOnLeave = false;
	this.AutoSizeColumns  = AutoSizeColumns;
	this.CancelCurrentActions = CancelCurrentActions; 		
	this.DeleteRows = DeleteRows;
	this.DoPostBack = DoPostBack; 
	this.FindHiddenValuesByRow = FindHiddenValuesByRow; 
	this.FireEvent = FireEvent; 	
	this.HitTest = HitTest; 
	this.Initialize = Initialize;
	this.MoveNext = MoveNext;
	this.MovePrevious = MovePrevious;
	this.ReportRowsStatus = ReportRowsStatus;
	this.ResumeEditOperation = ResumeEditOperation; 
	this.ResumeFilterOperation = ResumeFilterOperation;
	this.RetrieveRow = RetrieveRow;	
	this.UpdateData = UpdateData; 	
	this.getClassName = getClassName;
	this.getClientEventsCount = getClientEventsCount; 
	this.getChildLoadingMode = getChildLoadingMode; 
	this.getColumnSetNavigation = getColumnSetNavigation; 
	this.getEditControl = getEditControl; 
	this.getFilterMode = getFilterMode; 
	this.getFilterRow = getFilterRow; 	
	this.getFixTableSize = getFixTableSize; 
	this.getResetFilterCommand = getResetFilterCommand; 
	this.getID = getID; 	
	this.getIsInitialized = getIsInitialized; 
	this.getSelectOnExpand = getSelectOnExpand; 
	this.getSelectedClassName = getSelectedClassName;
	this.getGroupByBox = getGroupByBox;	
	this.getRootTable = getRootTable;
	this.getColumnAutoResize = getColumnAutoResize; 
	this.getGridEXRow = getGridEXRow;	
	this.isFiltered = isFiltered; 
	this.isHierarchicalGrid = isHierarchicalGrid; 
	this.getHtmlGridEX = getHtmlGridEX; 
	this.getHtmlHeight  = getHtmlHeight; 	
	this.getHtmlWidth = getHtmlWidth; 
	this.getNewRecord = getNewRecord; 
	this.getNextVisibleRow = getNextVisibleRow; 
	this.getPixelLeft = getPixelLeft; 
	this.getPixelTop = getPixelTop;
	this.getPreviousVisibleRow = getPreviousVisibleRow;
	this.getRecordCount = getRecordCount;
	this.getResizeGroups = getResizeGroups; 
	this.getResizeWidth = getResizeWidth; 
	this.getRowInIndex = getRowInIndex; 
	this.getRow = getRow; 	
	this.getRowByID = getRowByID; 
	this.getRowsInPageCount = getRowsInPageCount; 		
	this.getSelectionMode = getSelectionMode; 
	this.getSelectedItems = getSelectedItems;	
	this.getServerID = getServerID; 
	this.getTableDefinition = getTableDefinition;
	this.getTables = getTables;
	this.getThemedAreas = getThemedAreas;	
	this.getUpdateOnLeave = getUpdateOnLeave; 
	this.getUpdateMode = getUpdateMode; 
	this.getUseHiddenColumns = getUseHiddenColumns; 
	this.getVisibleHeight = getVisibleHeight; 	
	this.isDropDown = isDropDown;	
	this.setHitTestArea = setHitTestArea; 
	this.setCurrentRow = setCurrentRow; 	
	this.setOwner = setOwner; 				
	this.TabElementChanging = TabElementChanging; 	
	this.gridEX_onblur = gridEX_onblur;	
	this.gridEX_onkeydown = gridEX_onkeydown; 	
	this.gridEX_onmousemove = gridEX_onmousemove; 
	this.gridEX_onmouseup = gridEX_onmouseup; 
	this.gridEX_onresize = gridEX_onresize;		
	if(clientDefinition != null)
	{
		rowcount = clientDefinition[0]; 
		recordcount = clientDefinition[1]; 
		isfiltered = (clientDefinition[2] == 1) ? true : false; 
		columnAutoResize = (clientDefinition[3] == 1) ? true : false; 
		resizeHeight = (clientDefinition[4] == 1) ? true : false;
		resizeWidth = (clientDefinition[5] == 1) ? true :false;
		useHiddenColumns = (clientDefinition[6] == 1) ? true : false; 
		childLoadingMode = clientDefinition[7]; 
		selectionMode = clientDefinition[8];
		if(clientDefinition[9] != null)
		{
			this.ddbimg = clientDefinition[9][0]; 
			this.ddbpimg = clientDefinition[9][1]; 
		}	
		selectonexpand = (clientDefinition[10] == 1) ? true : false; 
		hierarchicalGrid = (clientDefinition[11] == 1) ? true : false;
		resizeGroups = (clientDefinition[12] == 1) ? true : false; 
		initRowID = clientDefinition[13];
		clientWidth = clientDefinition[14];
		columnSetNavigation = clientDefinition[15]; 
		isdropdown = (clientDefinition[16] == 1) ? true : false;		
		controlsToBuild = clientDefinition[17];
		formID = clientDefinition[18]; 
		updateMode = clientDefinition[19];
		updateOnLeave = (clientDefinition[20] == 1) ? true : false; 
		filterMode = clientDefinition[21]; 	
		this.selpb = (clientDefinition[22] == 1) ? true : false;
		this.ddpb = (clientDefinition[23] == 1) ? true: false;
		this.cmpb = (clientDefinition[24] == 1) ? true : false; 
		this.dcpb = (clientDefinition[25] == 1) ? true : false; 
		this.focusCss = (clientDefinition[26] == null) ? null : clientDefinition[26][0];
		this.focusRowCss = (clientDefinition[26] == null) ? null : clientDefinition[26][1];
		this.rowheaders = clientDefinition[27];
	}	
	if(columnAutoResize)
		htmlGridEXParent = getGridEXOffsetParent(htmlGridEX); 	
	
	function getRowByID(rowID)
	{
		if(gridEXRows == null)
			return null; 
		
		var l = gridEXRows.length; 
		var row = null; 			
		for(var i = 0; i < l; i++)
		{		
			row = gridEXRows[i]; 
			if(row.getID() == rowID)
				return row; 
		}				
		return null; 
	}		
	function AutoSizeColumns()
	{	  
		if(rootTable != null)	
		{
			rootTable.AutoSizeColumns();
			var _input = document.getElementsByName(getID() + "_clientwidth")[0];
			_input.value = getHtmlWidth(); 			
		}
	}	
	function DoPostBack(eventTarget, eventArgument,updateData)
	{
		if(updateData != null && updateData)
			UpdateData(false); 
		ReportRowsStatus(); 
		ReportEditOperation(eventArgument); 
		if(eventTarget == null)
			__doPostBack(getServerID(), eventArgument); 
		else
			__doPostBack(eventTarget, eventArgument); 
	}
	function FindHiddenValuesByRow(row)
	{
		if(getUseHiddenColumns() && hiddenValuesCollection != null)
		{
			var _hiddenIndex = 0; 
			var _hiddenValues = null; 
			var _hiddenValuesLength = hiddenValuesCollection.length; 
			var _index = 0; 
			var _table = null; 
			while(_index < _hiddenValuesLength)
			{
				if(hiddenValuesCollection[_index] == row.getID())
				{
					if(_table == null || _table.getID() != hiddenValuesCollection[_index+1])
						_table = getTables().getTableByID(hiddenValuesCollection[_index+1]);						
					
					_hiddenValues = new Array(); 					
					for(var _hiddenIndex = _index + 2; _hiddenIndex <= (_index + _table.getColumns().HiddenColumnsCount() * 2); _hiddenIndex = _hiddenIndex + 2)
					{
						_hiddenValues[_hiddenValues.length] = hiddenValuesCollection[_hiddenIndex]; 
						_hiddenValues[_hiddenValues.length] = hiddenValuesCollection[_hiddenIndex+1];						
					}					
					return _hiddenValues; 
				}
				else
				{
					if(_table == null || _table.getID() != hiddenValuesCollection[_index+1])
						_table = getTables().getTableByID(hiddenValuesCollection[_index+1]);
						
					_index += (_table.getColumns().HiddenColumnsCount() * 2) + 1;
					_index++; 
				}
			}			
			return null; 
		}
		throw Error("invalid operation"); 		
	}	
	function FixedResize()
	{		
		if(rootTable != null)
			rootTable.FixedResize(); 
	}	
	function CancelCurrentActions()
	{
		cancelCurrentUIEvents();
	}		
	function FireEvent(eventname, eventparams)
	{	
		if(eventhandlers == null || eventhandlers.length == 0)
			return null;
			
		var l = eventhandlers.length; 
		var _eventhandler = "";		
		for(var i = 0; i <l; i = i + 2)
		{
			if(eventhandlers[i] == eventname) 
			{
				_eventhandler = eventhandlers[i+1];
				i = l; 
			}
		}
		if(_eventhandler != "")
		{	
			var _params = "";
			var l = eventparams.length; 
			for(var i = 0; i < l; i++)
			{
				if(_params != "") _params += ",";
				_params += "eventparams[" + i + "]"; 
			}		
			var _cmd = "return eval(" + _eventhandler + "("  + _params + "))";
			var _function = new Function("eventparams", _cmd);
			return _function(eventparams);
		}
		return null; 
	}			
	function FilterData()
	{
		var _xml = getFilterXML(getFilterRow());   
		if(_xml.length > 0)
		{		
			var _input = document.getElementsByName(getID() + "_editinfo")[0];
			if(_input == null)
				throw Error("unable to find edit info field"); 
				
			_input.value = _xml;			
			DoPostBack(null, "ResumeFiltering"); 
			window.event.returnValue = false;
			window.event.cancelBubble = true;						
		}
	}
	function groupCellChanged(record)
	{
		if(record == null)
			return false;
			
		if(!record.getDataChanged())
			return false; 
			
		for(var i=0; i<record.getCellsLength();i++)
		{
			var cell = record.getCellByIndex(i);
			if(cell.getDataChanged() && cell.getColumn().isGrouped)
				return true; 
		}
		return false; 
	}
	function UpdateData(submit,resetAction)
	{	
		var result = false; 
		if(currentEditAction == 1)
			result = ResumeAddRecord(submit); 
		else
		{
			if(getUpdateMode() == 2)	
			{
				var s = groupCellChanged(getGridEXRow());		
				result = ResumeEditBatchRecord();
				if(s)
					DoPostBack(null, "ResumeEditing"); 
			}
			else
				result = ResumeEditRecord(submit); 
		}
		if(resetAction == null || resetAction)			
			currentEditAction = -1;
		return result; 
	}	
	function getInnerItemRow(row)
	{	
		var _innerItemRow = null; 
		var _tableID = null;
		if(isHierarchicalGrid())
		{			
			var _tmpTable = row.cells[0].childNodes[0]; 
			var _tmpRow = _tmpTable.rows[0]; 
			var _tmpCellsLength = _tmpRow.cells.length; 
			for(var _tmpCell = 0; _tmpCell < _tmpCellsLength && _innerItemRow == null; _tmpCell++)
			{
				if(_tmpRow.cells[_tmpCell].childNodes[0].tagName == "TABLE")
					_innerItemRow = _tmpRow.cells[_tmpCell].childNodes[0].rows[0]; 
			}			
			if(_innerItemRow == null)
				throw Error("unable to retrieve inner row");
		}		
		if(row.getAttribute("t") != null)
			_tableID = row.getAttribute("t"); 
		
		if(_tableID != null && getTables().getTableByID(_tableID).getUseColumnSets())
		{
			if(_innerItemRow == null)
				_innerItemRow = row;
				
			if(_innerItemRow.getAttribute("type") == null || _innerItemRow.getAttribute("type") == "3" || _innerItemRow.getAttribute("type") == "4" || _innerItemRow.getAttribute("type") == "9")				
				return _innerItemRow.cells[0].childNodes[0].rows[0]; 
			else
				return _innerItemRow; 
		}
		else if(isHierarchicalGrid() && _innerItemRow != null)
			return _innerItemRow;
		else
			return row; 
	}	
	function DeleteRows()	
	{	
		if(getGridEXRow().getType() != 3)
			return; 
			
		if(!getGridEXRow().getTable().getAllowDelete())
			return; 	
		
		ResumeDeleteRecord();
	}
	function moveToNextFocusCell(row)
	{
		if(row.getRowType() == "Record" || row.getRowType() == "NewRecord" || row.getRowType() == "FilterRow")
		{
			if(row.getTable().tabCellsOrder != null && row.getTable().tabCellsOrder.length > 0)
				row.setCurrentCell(row.getCellByColumnID(row.getTable().tabCellsOrder[1])); 
		}
	}
	function MoveNext()
	{		
		var nextRow = getGridEXRow().getNextRow();				
		if(nextRow != null && nextRow.getIsVisible()) 
		{					
			if(!nextRow.getVisibleInScroll())
			{
				var scrollTop = getRootTable().getHtmlItemsTable().offsetParent.scrollTop;
				getRootTable().getHtmlItemsTable().offsetParent.scrollTop = scrollTop +  nextRow.getRowHeight(); 
			}			
			setCurrentRow(nextRow); 
			getSelectedItems().SelectRow(nextRow);
			if(gridEX.currentCell != null)
			{
				if(gridEX.currentCell.getRow().getTable() == getGridEXRow().getTable())
				{
					if(getGridEXRow().getRowType() == "Record" || getGridEXRow().getRowType() == "NewRecord" || getGridEXRow().getRowType() == "FilterRow")
						getGridEXRow().setCurrentCell(getGridEXRow().getCellByColumnID(gridEX.currentCell.getColumn().getClientID()));
					else
						gridEX.currentCell.getInnerCell().setAttribute("className", gridEX.currentCell.cssName);
				}
				else
				{
					gridEX.currentCell.getRow().setCurrentCell(null);
					gridEX.currentCell = null;
					moveToNextFocusCell(getGridEXRow()); 											 
				}
			}
			else
				moveToNextFocusCell(getGridEXRow()); 
		}		
	}
	function MoveNextFocusCell()
	{
		getGridEXRow().NextFocusCell();
	}
	function MovePreviousFocusCell()
	{
		getGridEXRow().PreviousFocusCell();
	}
	function moveToPreviousFocusCell(row)
	{
		if(row.getRowType() == "Record" || row.getRowType() == "NewRecord" || row.getRowType() == "FilterRow")
		{
			if(row.getTable().tabCellsOrder != null && row.getTable().tabCellsOrder.length > 0)
				row.setCurrentCell(row.getCellByColumnID(row.getTable().tabCellsOrder[tabCellsOrder.length-1]));
		}
	}
	function MovePrevious()
	{
		var previousRow = getGridEXRow().getPreviousRow(); 
		if(previousRow != null && previousRow.getIsVisible())
		{
			if(!previousRow.getVisibleInScroll())
			{
				var scrollTop = getRootTable().getHtmlItemsTable().offsetParent.scrollTop;
				getRootTable().getHtmlItemsTable().offsetParent.scrollTop = scrollTop - previousRow.getRowHeight(); 				
			}			
			setCurrentRow(previousRow); 
			getSelectedItems().SelectRow(previousRow);
			if(gridEX.currentCell != null)
			{
				if(gridEX.currentCell.getRow().getTable() == getGridEXRow().getTable())
				{
					if(getGridEXRow().getRowType() == "Record" || getGridEXRow().getRowType() == "NewRecord" || getGridEXRow().getRowType() == "FilterRow")
						getGridEXRow().setCurrentCell(getGridEXRow().getCellByColumnID(gridEX.currentCell.getColumn().getClientID()));
					else
						gridEX.currentCell.getInnerCell().setAttribute("className", gridEX.currentCell.cssName);
				}
				else
				{
					gridEX.currentCell.getRow().setCurrentCell(null);
					gridEX.currentCell = null; 
					moveToPreviousFocusCell(getGridEXRow()); 
				}
			}
			else
				moveToPreviousFocusCell(getGridEXRow()); 
		}
	}	
	function MoveScrollTop()
	{
		var bottomRow = null; 
		var topRow = getRootRowFromInner(getGridEXRow().getInnerRow()); 
		var top = topRow.offsetTop; 
		var bottom = top - getRootTable().getHtmlItemsTable().offsetParent.offsetHeight;
		bottom = (bottom < 0) ? 0 : bottom; 
		var isBottom = false;
		var tmpRow = null; 
		var rowIndex = topRow.rowIndex - 1; 
		while(!isBottom && rowIndex >= 0)
		{
			tmpRow = getRootTable().getHtmlItemsTable().rows[rowIndex]; 
			if(tmpRow.getAttribute("type") != "1" && tmpRow.getAttribute("type") != "2" && tmpRow.getAttribute("type") != "6" && tmpRow.getAttribute("type") != "7" && tmpRow.getAttribute("type") != "10")
			{
				if(tmpRow.style.display != "none" && tmpRow.offsetTop < bottom)
					isBottom = true; 
				else if(tmpRow.style.display != "none")
					bottomRow = tmpRow; 					
			}
			rowIndex--; 			
		}
		if(isBottom || bottomRow != null)
		{
			if((bottomRow.offsetTop - getRootTable().getHtmlItemsTable().offsetParent.scrollTop) < 0)
				getRootTable().getHtmlItemsTable().offsetParent.scrollTop -= top - bottomRow.offsetTop; 
				
			var bottomGridEXRow = RetrieveRow(bottomRow, getInnerItemRow(bottomRow), null);
			setCurrentRow(bottomGridEXRow); 
			getSelectedItems().SelectRow(bottomGridEXRow); 				
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false;			
		}
		return true; 	
	}	
	function MoveScrollBottom()
	{
		var bottomRow = null; 
		var topRow = getRootRowFromInner(getGridEXRow().getInnerRow());
		var top = topRow.offsetTop; 		
		var bottom = top + getRootTable().getHtmlItemsTable().offsetParent.offsetHeight; 
		var isBottom = false; 
		var tmpRow = null; 
		var _rowsLength = getRootTable().getHtmlItemsTable().rows.length; 
		var rowIndex = topRow.rowIndex + 1; 
		while(!isBottom && rowIndex < _rowsLength)
		{
			tmpRow = getRootTable().getHtmlItemsTable().rows[rowIndex];
			if(tmpRow.getAttribute("type") != "1" && tmpRow.getAttribute("type") != "2" && tmpRow.getAttribute("type") != "6" && tmpRow.getAttribute("type") != "7" && tmpRow.getAttribute("type") != "10") // tmpRow.type != "4" &&
			{				
				if(tmpRow.style.display != "none" && (tmpRow.offsetTop + tmpRow.offsetHeight > bottom))
					isBottom = true; 
				else if(tmpRow.style.display != "none") 
					bottomRow = tmpRow; 
			}
			rowIndex++; 
		}		
		if(isBottom || bottomRow != null)
		{
			if(bottomRow.offsetTop >= getRootTable().getHtmlItemsTable().offsetParent.offsetHeight  + getRootTable().getHtmlItemsTable().offsetParent.scrollTop)
				getRootTable().getHtmlItemsTable().offsetParent.scrollTop += bottomRow.offsetTop - top; 
				
			var bottomGridEXRow = RetrieveRow(bottomRow, getInnerItemRow(bottomRow), null);
			setCurrentRow(bottomGridEXRow); 
			getSelectedItems().SelectRow(bottomGridEXRow);			
			window.event.returnValue = false; 
			window.event.cancelBubble = true; 
			return false; 			
		}		
		return true; 
	}	
	function isDropDown()
	{
		return isdropdown; 
	}
	function isFiltered()
	{
		return isfiltered; 
	}
	function isVisible() 
	{ 
		if(isDefaultView)
			return (document.defaultView.getComputedStyle(htmlGridEX, null).getPropertyValue("visibility") != "hidden" && document.defaultView.getComputedStyle(htmlGridEX, null).getPropertyValue("display") != "none" && htmlGridEX.offsetWidth > 0);
		else
			return (htmlGridEX.style.visibility != "hidden" && htmlGridEX.style.display != "none" && htmlGridEX.offsetWidth > 0); 
	}	
	function getClientEventsCount()
	{
		if(clientEventsCount == -1)
		{	
			if(eventhandlers != null && eventhandlers.length > 0)
				clientEventsCount = (eventhandlers.length / 2); 
			else
				clientEventsCount = 0; 
		}		
		return clientEventsCount; 
	}		
	function getColumnAutoResize()
	{
		return columnAutoResize; 
	}	
	function getColumnSetNavigation()
	{
		return columnSetNavigation; 
	}	
	function getChildLoadingMode()
	{
		return childLoadingMode; 
	}	
	function getEditControl(controlType, controlID)
	{		
		if(editControls == null || editControls.length == 0)
			throw Error("editControls collection is null or invalid"); 
			
		var _control = null; 
		var controlsLength = editControls.length; 
		for(var control = 0; control < controlsLength; control = control + 2)
		{			
			if(editControls[control] == controlType)
			{
				_control = editControls[control+1]; 
				if(controlID != null)
				{				
					if(_control.getID != null && _control.getID() == controlID)
						return _control; 
				}				
				else			
					return _control; 
			}
		}		
		throw Error("argument out of range"); 
	}	
	function getFilterMode()
	{
		return filterMode; 
	}	
	function getFilterRow()
	{	
		if(gridEXRows == null)
			return null; 
			
		var _rowslength = gridEXRows.length;
		for(var index = 0; index < _rowslength; index++)
		{		
			if(gridEXRows[index].getType() == 11)
				return gridEXRows[index]; 
		}		
		return getRowFromTop(9); 
	}	
	function getFixTableSize()
	{
		return fixTableSize; 
	}
	function HitTest()
	{
		switch(hitArea)
		{	
			case 1:
				return "GroupByBox";
			case 2:
				return "GroupByBoxInfoText"; 
			case 4:
				return "RowHeader"; 
			case 5:
				return "ColumnHeader";
			case 6:
				return "ColumnSetHeader"; 
			case 8:
				return "Cell"; 
			case 9:
				return "GroupRow"; 
			case 10:
				return "GroupTotalRow";
			case 11:
				return "TotalRow"; 
			case 12:
				return "NewRowCell"; 
			case 13:
				return "FilterRow"; 
			case 14:
				return "PreviewRow"; 
			default:
				return "Nothing"; 
		}
	}
	function getResetFilterCommand()
	{
		if(resetFilterCommand == null)
		{
			resetFilterCommand = new ResetFilterCommand(); 
			resetFilterCommand.Init(getID()); 
		}
		return resetFilterCommand; 
	}
	function getNewRecord()
	{					
		if(getRootTable() == null)
			return null;
			
		if(getRootTable().getNewRowPosition() == 2 && gridEXRows != null)
		{			
			var _rowslength = gridEXRows.length;
			var tmprow = null; 
			for(var index=0; index < _rowslength; index++)
			{
				tmprow = gridEXRows[index]; 
				if(tmprow.getType() == 9 && tmprow.getTable().getParent() == null)
					return tmprow;
			}			
		}
		return getRowFromTop(7); 
	}	
	function getRowFromTop(rowType)
	{	
		var _div;
		var _divs;
		var _divslength; 
		_div = null; 
		_divs = getRootTable().getHtmlDiv().all.tags("DIV"); 
		_divslength = _divs.length; 
		for(var idiv = 0; idiv < _divslength; idiv++)
		{
			_div = _divs[idiv]; 
			if(_div.getAttribute("type") != null && parseInt(_div.getAttribute("type"), 10) == rowType)
			{
				if(_div.childNodes[1].tagName == "TABLE")
				{
					if(_div.childNodes[1].rows.length > 0)
						return RetrieveRow(_div.childNodes[1].rows[0], getInnerItemRow(_div.childNodes[1].rows[0]), getRootTable(), 0); 							
				}
			}
		}
		return null; 
	}		
	function isHierarchicalGrid()
	{
		return hierarchicalGrid; 
	}	
	function getID()
	{
		return id; 
	}	
	function getIsInitialized()
	{
		return initialized; 
	}	
	function getServerID()
	{
		return serverid;
	}	
	function getGroupByBox()
	{
		return groupByBox; 
	}	
	function getPixelTop()
	{
		var _top = 0; 
		var _parent = htmlGridEX;
		while(_parent != null)
		{
			_top = _top + _parent.offsetTop; 
			_parent = _parent.offsetParent; 
		}
		if(htmlGridEX.offsetParent.scrollTop != 0)
			_top -= htmlGridEX.offsetParent.scrollTop; 
		return _top; 				
	}	
	function getPixelLeft()
	{
		var _left = 0;
		var _parent = htmlGridEX;
		while(_parent != null)
		{
			_left = _left + _parent.offsetLeft;
			_parent = _parent.offsetParent; 
		}
		return _left; 
	}	
	function getRootTable()
	{
		return rootTable;
	}
	function getSelectOnExpand()
	{
		return selectonexpand; 
	}
	function getGridEXRow()
	{
		return gridEXRow; 
	}	
	function getNextVisibleRow(rowIndex, rowPosition)
	{	
		var _innerRow = null; 
		var _innerType = null; 
		var _rowsLength = getRootTable().getHtmlItemsTable().rows.length; 
		while(rowIndex < _rowsLength)
		{
			_innerRow = getRootTable().getHtmlItemsTable().rows[rowIndex]; 
			_innerType = _innerRow.getAttribute("type"); 
			if(_innerType != "1" && _innerType != "2" && _innerType != "6" && _innerType != "7" && _innerType != "10")
			{
				if(_innerRow.style.display != "none")
					return RetrieveRow(_innerRow, getInnerItemRow(_innerRow), null, rowPosition); 
				else
					rowPosition++; 
			}
			rowIndex++; 
		}
		return null; 
	}	
	function getPreviousVisibleRow(rowIndex, rowPosition)
	{			
		var _innerRow = null; 				
		if(rowIndex < 0) 
		{	
			if(rowPosition == 0 && getFilterMode() == 1)
				return getFilterRow();
			else if(rowPosition == 0 && getRootTable().getParent() == null && getRootTable().getNewRowPosition() == 2)
				return getNewRecord(); 				
			else if(rowPosition == 1 && getFilterMode() == 1 && getRootTable().getParent() == null && getRootTable().getNewRowPosition() == 2)
				return getNewRecord();							
		}		
		var _innerType = null; 
		while(rowIndex >= 0)
		{			
			_innerRow = getRootTable().getHtmlItemsTable().rows[rowIndex]; 
			_innerType = _innerRow.getAttribute("type"); 
			if(_innerType != "1" && _innerRow.getAttribute("type") != "2" && _innerType != "6" && _innerType != "7" && _innerType != "10")
			{
				if(_innerRow.style.display != "none")
					return RetrieveRow(_innerRow, getInnerItemRow(_innerRow), null, rowPosition); 
				else
					rowPosition--; 
			}
			rowIndex--; 
		}			
		return null; 
	}
	function getRowInIndex(i)
	{
		if(i < 0 || gridEXRows == null || i > gridEXRows.length)
			throw Error("argument index out of range"); 
			
		return gridEXRows[i]; 
	}
	function getRow(position)
	{
		if(position < 0 || position >= getRowsInPageCount())
			throw Error("argument position out of range");
		
		var rowslength = -1; 
		if(gridEXRows != null)
		{
			var _gridexrow = null; 
			rowslength = gridEXRows.length; 
			for(var irow = 0; irow < rowslength; irow++)
			{				
				_gridexrow = gridEXRows[irow]; 
				if(_gridexrow.getPosition() == position)
					return _gridexrow;
			}
		}				
		if(position == 0 || position == 1)			
		{
			if(position == 0 && getFilterMode() == 1)
				return getFilterRow(); 
			else if(position == 0 && getRootTable().getParent() == null && getRootTable().getNewRowPosition() == 2)
				return getNewRecord(); 
			else if(position == 1 && getFilterMode() == 1 && getRootTable().getParent() == null && getRootTable().getNewRowPosition() == 2)
				return getNewRecord(); 							
		}				
		var positionToFind = position; 
		if(getRootTable().getParent() == null)
		{ 
			if(getRootTable().getNewRowPosition() == 2)
				positionToFind--; 
			if(getFilterMode() == 1)
				positionToFind--;
		}
		var _innerRow = null; 
		var _indexPos = 0; 
		rowslength = getRootTable().getHtmlItemsTable().rows.length; 
		for(var irow = 0; irow < rowslength; irow++)
		{
			_innerRow = getRootTable().getHtmlItemsTable().rows[irow]; 
			if(_innerRow.getAttribute("type") != "1" && _innerRow.getAttribute("type") != "2" && _innerRow.getAttribute("type") != "6" && _innerRow.getAttribute("type") != "7" && _innerRow.getAttribute("type") != "10")
			{
				if(_indexPos == positionToFind)
					return RetrieveRow(_innerRow, getInnerItemRow(_innerRow), null, position); 
				else
					_indexPos++; 
			}
		}				
		throw Error("argument position out of range"); 
	}			
	function getRowsInPageCount()
	{
		return rowcount; 
	}	
	function getRecordCount()
	{
		return recordcount; 
	}
	function getSelectionMode()
	{
		return selectionMode; 
	}	
	function getThemedAreas()
	{
		return 1; 
	}	
	function getUseHiddenColumns()
	{
		return useHiddenColumns; 
	}		
	function getClassName(rowID)
	{		
		var l = arrRowsCss.length; 
		for(var i = 0; i < l; i = i + 3)
		{			
			if(arrRowsCss[i] == rowID)
				return arrRowsCss[i+1]; 
		}		
		return null; 
	}	
	function getSelectedClassName(rowID)
	{
		var l = arrRowsCss.length; 
		for(var i = 0; i < l; i = i + 3)
		{
			if(arrRowsCss[i] == rowID)
				return arrRowsCss[i+2]; 
		}		
		return null; 
	}	
	function getSelectedItems()
	{
		return selectedItemsCollection;
	}	
	function getTables()
	{		
		if(tables == null)
		{
			tables = new GridEXTableCollection(gridEX); 
			tables.Add(rootTable);
			copyChildTables(tables, rootTable); 
		}
		return tables; 
	}	
	function getTableDefinition()
	{
		return tableDefinition;
	}	
	function getUpdateMode()
	{
		return updateMode; 
	}	
	function getUpdateOnLeave()
	{
		return updateOnLeave; 
	}		
	function setOwner(value)
	{
		owner = value; 
	}	
	function getFilterXML(_gridEXRow)
	{
		if(_gridEXRow == null)
			return; 
			
		if(_gridEXRow.getType() != 11)
			throw Error("invalid operation exception: row is not a filter row");
			
		var _editInnerXML = "";
		var _editOuterXML = "";
		var _cell = null;
		var _cellsLength = _gridEXRow.getCellsLength(); 
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			_cell = _gridEXRow.getCellByIndex(icell); 
			if(_cell.getDataChanged() || _cell.getText() != "") 
			{				
				if(_cell.getValue() == null || _cell.getText() == "" || _cell.getInnerCell().getAttribute("remfilter") != null)
					_editInnerXML += "[tag:]cell id='" +  _cell.getColumn().getClientID() + "' action='-1' /[:tag]";
				else
				{			
					if(_cell.getInnerCell().getAttribute("niv") != null && _cell.getInnerCell().getAttribute("niv") != "")
						_editInnerXML += "[tag:]cell id='"+_cell.getColumn().getClientID()+"' niv='1'[:tag]"+encodeURI(_cell.getInnerCell().getAttribute("niv"))+"[tag:]/cell[:tag]";
					else
					{
						_editInnerXML += "[tag:]cell id='" +  _cell.getColumn().getClientID() + "'[:tag]";				
						_editInnerXML += encodeURI(_cell.getValue()); 
						_editInnerXML += "[tag:]/cell[:tag]";
					}
				}
			}			
		}
		if(_editInnerXML.length > 0)
		{
			_editOuterXML += "[tag:]row action='5' id='" + _gridEXRow.getID() + "' table='" + _gridEXRow.getTable().getID() + "'[:tag]"; 
			_editOuterXML += _editInnerXML; 
			_editOuterXML += "[tag:]/row[:tag]"; 
		}
		return _editOuterXML; 
	}	
	function getEditableXML(editAction, _gridEXRow)
	{
		var _editInnerXML = "";
		var _editOuterXML = ""; 		
		if(_gridEXRow == null)
		{
			_gridEXRow = getGridEXRow();					
			if(_gridEXRow == null)
				return "";
		}
		if((_gridEXRow.getType() == 3 || _gridEXRow.getType() == 9 || _gridEXRow.getType() == 11) &&_gridEXRow.getDataChanged())
		{
			var _cell = null;
			var l = _gridEXRow.getCellsLength(); 
			for(var i = 0; i < l; i++)
			{				
				_cell = _gridEXRow.getCellByIndex(i); 
				if((_cell.getDataChanged() || (_cell.getInnerCell() != null && _cell.getInnerCell().getAttribute("ind") != null)) && (!_cell.getColumn().getActAsSelector() || (_cell.getColumn().getActAsSelector() && _gridEXRow.getType() == 9 && _cell.getColumn().getEditType() == 9)))  //if(_cell.getDataChanged() && !_cell.getColumn().getActAsSelector())
				{
					if(_cell.getInnerCell() == null || _cell.getInnerCell().getAttribute("niv") == null || _cell.getInnerCell().getAttribute("niv") == "")
					{
						_editInnerXML += "[tag:]cell id='" + _cell.getColumn().getClientID() + "'[:tag]"; 					
						_editInnerXML += encodeURI(_cell.getValue()); 
						_editInnerXML += "[tag:]/cell[:tag]";
					}
					else
					{					
						_editInnerXML += "[tag:]cell id='" + _cell.getColumn().getClientID() + "' niv='1'[:tag]"; 
						_editInnerXML += encodeURI(_cell.getInnerCell().getAttribute("niv"));
						_editInnerXML += "[tag:]/cell[:tag]";
					}
				}
			}
			if(_editInnerXML.length > 0)
			{				
				_editOuterXML += "[tag:]row action='" + editAction + "' id='" + _gridEXRow.getID() + "' table='" + _gridEXRow.getTable().getID() + "'[:tag]"; 
				_editOuterXML += _editInnerXML; 
				_editOuterXML += "[tag:]/row[:tag]"; 
			}
		}
		return _editOuterXML; 
	}	
	function ResumeAddRecord(submit)	
	{
		if(getGridEXRow() != null)
		{
			var cancel = FireEvent("AddingRecord", [getGridEXRow()]);
			if(cancel != null && cancel)
				return false; 
		}
		if(getGridEXRow() == null || !getGridEXRow().getDataChanged())		
			return false; 
	
		var xml = getEditableXML(currentEditAction, null);
		if(xml.length > 0)
		{
			var _input = document.getElementsByName(getID() + "_editinfo")[0];
			if(_input == null)
				throw Error("unable to find edit info field"); 
				
			var isbatch = false;
			if(getUpdateMode() == 2)
			{
				var batchXML = ResumeEditBatchRecordCore(); 
				if(batchXML.length > 0)
				{
					isbatch = true; 
					xml = "[tag:]rows action='2'[:tag]" + batchXML + xml + "[tag:]/rows[:tag]";				
				}
			}
			_input.value = xml;			
			if(submit)
			{			
				DoPostBack(null, "ResumeEditing"); 
				window.event.returnValue = false;
				window.event.cancelBubble = true;
			}
			return true;	
		}
		return false; 
	}
	function RetrieveRow(rootRow, innerRow, table, position, findById)
	{
		var _rowID = rootRow.getAttribute("id");		
		var _row = null;
		if(findById == null || findById)
			_row = getRowByID(_rowID); 
		if(_row == null)
		{
			if(table == null)
				table = getTables().getTableByID(rootRow.getAttribute("t")); 
				
			if(innerRow == null)
				innerRow = getInnerItemRow(rootRow); 
				
			_row = new GridEXRow(_rowID, innerRow, table, position, rootRow); 
			if(gridEXRows == null)
				gridEXRows = new Array(); 
			
			rootRow.setAttribute("ri", gridEXRows.length);
			gridEXRows[gridEXRows.length] = _row;
		}
		return _row;
	}
	function ResumeEditBatchRecordCore()
	{
		var batchXML = ""; 
		var r = null; 
		var l = gridEXRows.length; 
		var xml = ""; 
		for(var i = 0; i < l; i++)
		{
			r = gridEXRows[i]; 			
			if(r.getType() == 3)
			{
				var cancel = FireEvent("UpdatingRecord",[r]); 
				if(cancel == null || !cancel)
				{
					if(r.getDataChanged())
					{
						xml = getEditableXML(2, r);
						if(xml.length > 0)
							batchXML += xml;
					}
				}
			}
		}
		return batchXML; 
	}
	function ResumeEditBatchRecord()
	{		
		if(gridEXRows == null || gridEXRows.length == 0)
			return false; 

		var batchXML = ResumeEditBatchRecordCore();
		if(batchXML.length > 0)
		{
			batchXML = "[tag:]rows action='2'[:tag]" + batchXML + "[tag:]/rows[:tag]";		
			var _input = document.getElementsByName(getID() + "_editinfo")[0]; 
			if(_input == null)
				throw Error("unable to find edit info field"); 
				
			_input.value = batchXML;
			return true;
		}
		return false; 
	}	
	function ResumeEditRecord(submit)
	{
		if(getGridEXRow() != null)
		{
			var cancel = FireEvent("UpdatingRecord", [getGridEXRow()]);
			if(cancel != null && cancel)
				return false;
		}	
		if(getGridEXRow() == null || !getGridEXRow().getDataChanged())
			return false; 
			
		var _xml = getEditableXML(currentEditAction, null);
		if(_xml.length > 0)
		{
			var _input = document.getElementsByName(getID() + "_editinfo")[0];
			if(_input == null)
				throw Error("unable to find edit info field"); 
				
			_input.value = _xml;
			if(submit)
			{
				DoPostBack(null, "ResumeEditing"); 
				window.event.returnValue = false;
				window.event.cancelBubble = true;
			}
			return true;
		}		
		return false; 
	}	
	function ResumeDeleteRecord()
	{
		var xml = ""; 
		var xmlitems = ""; 
		for(var i=0;i<getSelectedItems().Count(); i++)
		{
			var item = getSelectedItems().getSelectedItemInIndex(i); 
			var cancel = FireEvent("DeletingRecords", [item.getRow()]);
			if(cancel == null || !cancel)
				xmlitems += "[tag:]row action='3' id='" + item.getRow().getID() + "' table='" + item.getRow().getTable().getID() + "' /[:tag]"; 			
		}
		if(xmlitems.length > 0)
		{
			xml = "[tag:]rows action='3'[:tag]" + xmlitems + "[tag:]/rows[:tag]";				
			var _input = document.getElementsByName(getID() + "_editinfo")[0]; 
			if(_input == null)
				throw Error("unable to find edit info field"); 
			
			_input.value = xml; 
			DoPostBack(null, "ResumeEditing");
			window.event.returnValue = false;
			window.event.cancelBubble = true;
		}
	}
	function ReportEditOperation(arg)
	{
		if(arg == "Expand" || arg == "Collapse" || arg == "SelectionChanged")
		{
			if(getUpdateMode() == -1)
				return; 
				
			if(haltEdition)
				return;
				
			UpdateData(false); 
		}
	}
	function ResumeEditOperation()
	{	
		if(getUpdateMode() == -1)
			return false; 
		
		if(haltEdition)
			return false;			
			
		return UpdateData(true);										
	}	
	function ResumeFilterOperation()
	{		
		FilterData(); 
	}	
	function isEditing()
	{
		return (currentEditAction != -1 && gridEXRow != null);  // && currentRowID != null && currentRow != null);
	}	
	function isFiltering()
	{
		return (gridEXRow != null && gridEXRow.getType()  == 11); 
	}	
	function setCurrentRow(row, selectRow)
	{
		if(isEditing()) 
		{
			if(getGridEXRow() != row)
			{
				if(ResumeEditOperation())
				{	
					gridEXRow = row; 		
					return; 
				}
			}
		}
		else if(isFiltering())
		{
			if(getGridEXRow() != row && getGridEXRow().getDataChanged())
			{
				ResumeFilterOperation();
				gridEXRow = row; 
				return;
			}
		}		
		if(row.getTable().getAllowEdit() && (row.getType() == 9 || row.getType() == 3 || row.getType() == 4))
		{
			if(row.getType() == 9)
				currentEditAction = 1;
			else if(row.getType() == 3 || row.getType() == 4)
				currentEditAction = 2;
		}
		else
			currentEditAction = -1;
	
		var _oldrow = null; 
		if(gridEXRow != null && gridEXRow != row && gridEXRow.HideHeaderIndicator != null)
		{
			gridEXRow.HideHeaderIndicator(); 	
			_oldrow = gridEXRow.getInnerRow(); 				
		}			
		if(gridEXRow != row && getSelectionMode() != 3)
			FireEvent("SelectionChanged", [row]);	
			
		if(gridEXRow != null && gridEXRow.getType() == 9)
		{
			for(var i=0;i<gridEXRow.getCellsLength();i++)
			{
				var cell = gridEXRow.getCellByIndex(i);
				if(cell.getColumn().getVisible() && cell.getInnerCell().getAttribute("default") != null && !cell.getDataChanged())
					cell.setText(""); 				
			}
		}
		if(gridEXRow != null)
			gridEXRow.current = false;
		gridEXRow = row;
		gridEXRow.current = true; 
		if(selectRow != null && selectRow)
			getSelectedItems().SelectSingleRow(gridEXRow);
		if(gridEXRow != null && gridEXRow.getType() == 9)
		{
			for(var i=0;i<gridEXRow.getCellsLength();i++)
			{
				var cell = gridEXRow.getCellByIndex(i);
				if(cell.getColumn().getVisible() && cell.getInnerCell().getAttribute("default") != null)
				{
					if(cell.getColumn().getEditType() == 6 && !cell.getDataChanged())
					{
						var list = cell.getGridEX().getEditControl(6, cell.getColumn().getClientID() + "_ValueList");
						var text = list.getDisplayByValue(cell.getInnerCell().getAttribute("default")); 
						cell.setText(text); 
					}
					else if(cell.getColumn().getEditType() == 5 && !cell.getDataChanged())
					{
						var combo = cell.getGridEX().getEditControl(5, cell.getColumn().getClientID() + "_Combo");
						var text = combo.getDisplayByValue(cell.getInnerCell().getAttribute("default"));
						cell.setText(text); 
					}
					else if(cell.getColumn().getEditType() == 7 && !cell.getDataChanged())
					{
						var combo = cell.getGridEX().getEditControl(7, cell.getColumn().getDropDownID());
						var text = combo.getDisplayByValue(cell.getInnerCell().getAttribute("default")); 
						cell.setText(text); 
					}
					else if(cell.getColumn().getEditType() == 8 && !cell.getDataChanged())
					{
						var combo = cell.getGridEX().getEditControl(8, cell.getColumn().getDropDownID());
						var text = combo.getDisplayByValue(cell.getInnerCell().getAttribute("default"));
						cell.setText(text);						
					}					
					else if(cell.getColumn().getEditType() != -1 && !cell.getDataChanged())
						cell.setText(cell.getInnerCell().getAttribute("default"));
				}
			}
		}
		if(gridEXRow.ShowHeaderIndicator != null)
			gridEXRow.ShowHeaderIndicator();		
	}
	function setHitTestArea(area)
	{
		if(hitArea != area)
		{
			if(area == 0)
			{				
				if(getGridEXRow() != null)
				{					
					var rowType = getGridEXRow().getType(); 
					if(rowType == 3 || rowType == 4)
					{
						var tdType = getTypeOfTD(window.event.srcElement); 
						if(tdType == "rh")
							hitArea = 4;
						else if(getGridEXRow().getCellSelected() != null)
							hitArea = 8;
					}
					else if(rowType == 8)
						hitArea = 9;
					else if(rowType == 9)
					{						
						if(getGridEXRow().getCellSelected() != null)
							hitArea = 12;
					}
					else if(rowType == 11)
						hitArea = 13;
					else if(rowType == 12)
						hitArea = 10; 
					else if(rowType == 5)
						hitArea = 11; 
				}
			}
			else
				hitArea = area; 
		}
	}	
	function gridEX_onresize()
	{	
		if(!isVisible())
			return; 
				
		resizeMode = -1;
		if(resizeHeight)
			getRootTable().ResizeHeight();
			
		if(resizeWidth)
			AutoSizeColumns(); 
		
		try
		{	
			window.event.returnValue = true;
		} catch(err) { }
		return true; 
	}	
	function TabElementChanging(editing)
	{	
		if((getGridEXRow() != null && editing != null && editing) || isValidEventForRow())
			getGridEXRow().TabChanging(editing);	
	}	
	function isValidEventForRow()
	{
		if(getGridEXRow() != null)	 
				return true;
			
		return false; 
	}	
	function DefaultOnKeyDown()
	{		
		if(window.event.keyCode == 27)
			CancelCurrentActions();
		else if(window.event.keyCode == 9)
		{
			TabElementChanging(null);
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return cancelEvent();
		}
		else if(window.event.keyCode == 39)
		{
			if(getGridEXRow() != null)
			{
				if(getGridEXRow().getRowType() == "GroupHeader")
					getGridEXRow().Expanding();
				else if(getGridEXRow().getRowType() == "Record" && (!getGridEXRow().getTable().getAllowEdit() || window.event.shiftKey))
					getGridEXRow().Expanding();
				else if(isValidEventForRow())
				{
					MoveNextFocusCell();
					return cancelEvent();
				}					
			} 
			else if(isValidEventForRow())
			{
				MoveNextFocusCell();
				return cancelEvent();
			}
		}
		else if(window.event.keyCode == 37)
		{			
			if(getGridEXRow() != null)
			{
				if(getGridEXRow().getRowType() == "GroupHeader")
					getGridEXRow().Collapsing();
				else if(getGridEXRow().getRowType() == "Record" && (!getGridEXRow().getTable().getAllowEdit() || window.event.shiftKey))
					getGridEXRow().Collapsing();
				else if(isValidEventForRow())
				{
					MovePreviousFocusCell();
					return cancelEvent(); 
				}					
			}
			else if(isValidEventForRow())
			{
				MovePreviousFocusCell();
				return cancelEvent(); 
			}
		}
		else if(window.event.keyCode == 40) 
		{
			if(isValidEventForRow())
			{
				MoveNext();
				return cancelEvent(); 
			}
		}
		else if(window.event.keyCode == 38)
		{
			if(isValidEventForRow())
			{			
				MovePrevious();
				return cancelEvent(); 
			}
		}
		else if(window.event.keyCode == 33)
		{
			if(isValidEventForRow())
				MoveScrollTop(); 
		}
		else if(window.event.keyCode == 34)
		{
			if(isValidEventForRow())
				MoveScrollBottom(); 
		}
		else if(window.event.keyCode == 46)
		{
			if(isValidEventForRow())
				DeleteRows(); 
		}
	}	
	function gridEX_onblur()
	{	
		if(!htmlGridEX.contains(document.activeElement) || !htmlGridEX.contains(window.event.srcElement))
		{
			if(owner != null && owner.GridEX_OnBlur != null)
				owner.GridEX_OnBlur(); 
		}
	}	
	function gridEX_onkeydown()
	{		
		var cancel = FireEvent("KeyDown", [gridEX, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false; 
			window.event.cancelBubble = true;
			return false; 
		}		
		if(owner != null && owner.GridEX_OnKeyDown != null)
			owner.GridEX_OnKeyDown();
		else
			DefaultOnKeyDown();
	}	
	function gridEX_onmousemove()
	{
		var event = window.event; 
		if(columnResizing != null && columnResizing)
			showResizeLine(event);		
		else if(columnDraging)
		{
			drag_onmousemove(event); 
			event.cancelBubble = true; 
		}		
	}	
	function gridEX_onmouseup()
	{		
		if(columnResizing)
		{			
			if(columnresizeMode == 1)
				endColumnResize();
			else if(columnresizeMode == 2)
				endColumnSetResize(); 
		}
		else if(columnDraging)
		{
			drag_onmouseup(); 
			cancelColumnDraging();	
		}
	}
	function gridEX_onselectstart()
	{				
	}	
	function Initialize()
	{
		if(groupByBox != null || rootTable != null)
			throw Error("initialize operation could be performed only once"); 					
	
		groupByBox  = setGroupByBox(gridEX);		
		rootTable = setRootTable(gridEX);		
		if(isdropdown)
		{
			if(selectedItemsCollection == null)
				selectedItemsCollection = new GridEXSelectedItemCollection(gridEX, selectedItems); 
		}
	}	
	function ReportRowsStatus()
	{
		var rowstatus = ""; 
		var input = document.getElementsByName(getID() + "_rowstatus")[0]; 
		if(input == null)
			 throw Error("unable to find report row status"); 
			 
		if(gridEXRows != null)
		{					
			var _row = null;
			var _rowsLength = gridEXRows.length; 
			for(var irow = 0; irow < _rowsLength; irow++)
			{
				_row = gridEXRows[irow]; 
				if(_row.getType() == 3 || _row.getType() == 8)
				{					
					if(rowstatus.length > 0)
						rowstatus += "|"; 
					rowstatus += _row.ReportStatus(); 
				}				
			}
			if(rowstatus.length > 0)
				input.value = rowstatus; 				
		}
	}	
	function gridEX_onbeforeunload()
	{	
		window.removeEventListener("keydown", gridEX_onkeydown, false);
		window.removeEventListener("beforeunload", gridEX_onbeforeunload, false); 		
	}	
	function gridEX_onsubmit()
	{		
		if(getUpdateOnLeave())
			UpdateData(false); 			

		ReportRowsStatus();
		window.event.returnValue = true; 	
	}	
	function gridEX_onload()
	{			
		Initialize();				
		loadAdditionalElements(gridEX);
		if(controlsToBuild != null)
		{		
			editControls = new Array(); 			
			var controlToBuild = -1; 
			var controlToBuildID = null; 			
			for(var control = 0; control < controlsToBuild.length; control = control + 2)
			{
				controlToBuild = controlsToBuild[control];
				controlToBuildID = controlsToBuild[control+1]; 
				if(controlToBuild == 2 || controlToBuild == 9)
				{
					editControls[editControls.length] = controlToBuild;
					editControls[editControls.length] = new GridEXEditTextBox(controlToBuildID); 
				}
				else if(controlToBuild == 3)  
				{					
					editControls[editControls.length] = 3;
					editControls[editControls.length] = new GridEXCalendarDropDown(controlToBuildID, eval(controlToBuildID + "_months"), eval(controlToBuildID + "_firstDayOfWeek"));  
				}
				else if(controlToBuild == 4) 
				{
					editControls[editControls.length] = 4;
					editControls[editControls.length] = new GridEXCalendarComboDropDown(controlToBuildID,  eval(controlToBuildID + "_months"), eval(controlToBuildID + "_firstDayOfWeek"));
				}				
				else if(controlToBuild == 5) 
				{
					editControls[editControls.length] = 5; 
					editControls[editControls.length] = new GridEXCombo(controlToBuildID);  
				}
				else if(controlToBuild == 6) 
				{
					editControls[editControls.length] = 6;
					editControls[editControls.length] = new GridEXValueList(controlToBuildID); 
				}
				else if(controlToBuild == 7) 
				{
					editControls[editControls.length] = 7; 
					editControls[editControls.length] = new GridEXComboDropDown(controlToBuildID); 
				}
				else if(controlToBuild == 8) 
				{
					editControls[editControls.length] = 8; 
					editControls[editControls.length] = new GridEXDropDown(controlToBuildID);  
				}
				else if(controlToBuild == 14) 
				{
					editControls[editControls.length] = 14; 
					editControls[editControls.length] = new GridEXEditTextArea(controlToBuildID); 
				}
			}
		}
		if(resizeHeight)
		{
			if(!isVisible())
				return; 
				
			getRootTable().ResizeHeight();
		}	
		if(getColumnAutoResize())
		{	
			if(!isVisible())
				return; 		
		
			if(clientWidth != -1 && clientWidth != getHtmlWidth())
				resizeMode = 1;
				
			AutoSizeColumns();			
			resizeMode = -1; 
		}
		else
		{	
			if(!isVisible())
				return; 
				
			if(clientWidth != -1 && clientWidth != getHtmlWidth())
				resizeMode = 1; 								

			resizeMode = -1; 
		}		
		if(selectedItemsCollection == null)
			selectedItemsCollection = new GridEXSelectedItemCollection(gridEX, selectedItems);
		var element = document.getElementsByName(getID() + "_scrollstatus")[0];
		if(element != null && element.value != "-1")
			getRootTable().getHtmlItemsTable().offsetParent.scrollTop = parseInt(element.value, 10); 
		if(initRowID != null && initRowID.length > 0)
		{
			var _initRootRow = document.getElementById(initRowID); 
			if(_initRootRow != null)
			{
				var _initRow = RetrieveRow(_initRootRow, getInnerItemRow(_initRootRow), null, null); 
				setCurrentRow(_initRow);
				if(!_initRow.getVisibleInScroll())
					getRootTable().getHtmlItemsTable().offsetParent.scrollTop = _initRootRow.offsetTop;
				if(htmlGridEX.setActive != null)
					htmlGridEX.setActive(); 
			}
		}		
		if(rv == null)
		{
			var i = window.navigator.userAgent.indexOf("rv:");
			if(i > 0)
			{
				var j = window.navigator.userAgent.indexOf(")", i);
				if(j > 0)
				try
				{
					rv = parseFloat(window.navigator.userAgent.substr(i+3, j-(i+3)));
				}
				catch(err) {}
			}
		}		
		FireEvent("GridEXLoad", [gridEX]);
		ReportRowsStatus(); 
		initialized = true;
		return true; 
	}	
	function body_onmousemove(event)
	{
		if(couldStartDrag && dragpoint != null)
		{		
			if(Math.abs(event.clientX - dragpoint.X()) > 4 || Math.abs(event.clientY - dragpoint.Y()) > 4)
			{	
				startColumnDrag(couldDragColumn, couldDragHeader, event);
				couldStartDrag = false;
				couldDragColumn = couldDragHeader = null; 
				dragpoint = null; 
			}			
			event.cancelBubble = true; 
		}
		else if(columnDraging)
		{
			drag_onmousemove(event);
			event.cancelBubble = true; 		
		}
	}	
	function body_onselectstart()
	{		
	}		
	function getHtmlGridEX()
	{
		return htmlGridEX; 
	}	
	function getHtmlWidth()
	{									
		var width = -1; 
		if(htmlGridEX.style.width != "" && htmlGridEX.style.width.indexOf("%") > 0)
			width = htmlGridEXParent.offsetWidth * (getPercentWidth(htmlGridEX.style.width) / 100); 
		else if(htmlGridEX.style.width.indexOf("px") != 0)
			width = getPixelWidth(htmlGridEX.style.width); 
		else
			width = htmlGridEX.offsetWidth; 				
		
		var _style = null;
		if(isDefaultView)
			_style = document.defaultView.getComputedStyle(htmlGridEX, null); 		
		
		if(htmlGridEXParent.tagName == "BODY" && htmlGridEX.style.width.indexOf("%") > 0)
		{
			var _parentstyle = null;
			if(isDefaultView) 
			{
				_parentstyle = document.defaultView.getComputedStyle(htmlGridEXParent, null); 					
				width -=  (getPixelWidth(_parentstyle.getPropertyValue("margin-left")) + getPixelWidth(_parentstyle.getPropertyValue("margin-right"))); 
			}
			else	
				width -= getPixelWidth(htmlGridEXParent.style.marginLeft) + getPixelWidth(htmlGridEXParent.style.marginRight); 
		}
		if(getRootTable().getHtmlItemsTable().offsetParent == null)
			return; 
			
		if(getRootTable().getHtmlItemsTable().offsetParent.scrollHeight > getRootTable().getHtmlItemsTable().offsetParent.offsetHeight)
		{
			if(getRootTable().getHtmlItemsTable().offsetHeight != getRootTable().getHtmlItemsTable().offsetParent.offsetHeight)
			{
				if(getRootTable().getHtmlItemsTable().getAttribute("empty") == null)
					width -= 17;
			}
		}
		var offset = 0; 
		if(_style != null)
		{
			offset += getPixelWidth(_style.getPropertyValue("border-left-width")); 
			offset += getPixelWidth(_style.getPropertyValue("border-right-width")); 				
		}
		else
		{
			offset += getPixelWidth(htmlGridEX.style.borderLeftWidth); 
			offset += getPixelWidth(htmlGridEX.style.borderRightWidth); 
		}
		return width - offset; 
	}	
	function getResizeGroups()
	{
		return resizeGroups; 
	}
	function getResizeWidth()
	{
		if(resizeMode == -1)
			return getHtmlWidth(); 
		else
			return clientWidth; 
	}	
	function getHtmlHeight(full)
	{
		if(full)
			return htmlGridEX.offsetHeight;
		else	
		{
			if(groupByBox != null)			
				return htmlGridEX.offsetHeight - groupByBox.getHtmlBox().offsetHeight;
		}
	}	
	function getVisibleHeight()
	{				
		var htmlparent = htmlGridEX.offsetParent;
		var visibleheight = htmlparent.clientHeight;
		return visibleheight;
	}	
	function setGroupByBox(gridex)
	{
		var divs = gridex.getHtmlGridEX().getElementsByTagName("DIV"); 
		if(divs != null && divs.length > 0)
		{
			var div = null; 
			for(var index = 0; index < divs.length; index++)
			{
				div = divs[index]; 
				if(div.getAttribute("type") != null && parseInt(div.getAttribute("type"), 10) == 5)
					return new GridEXGroupByBox(div, gridex); 
			}
		}
		return null; 
	}	
	function setRootTable(gridex)
	{				
		var divs = gridex.getHtmlGridEX().getElementsByTagName("DIV"); 
		if(divs != null && divs.length > 0)
		{
			var _div = null; 
			for(var index = 0; index <  divs.length; index++)
			{				
				_div = divs[index]; 				
				if(_div.getAttribute("type") != null && parseInt(_div.getAttribute("type"), 10) == 3)
				{										
					var table = new GridEXTable(_div, _div.id, null, 0, gridex);
					return table; 
				}
			}
		}
		else
			return null;		
	}	
	function copyChildTables(tables, table)
	{		
		var childTables = table.getChildTables();
		if(childTables == null)
			return;
			
		var _table = null; 
		for(var ichild = 0; ichild < childTables.Count(); ichild++)
		{
			_table = childTables.getTableInIndex(ichild); 
			tables.Add(_table);
			copyChildTables(tables, _table); 
		}		
	}		
	window.addEventListener("keydown", gridEX_onkeydown, false);
	if(!isDropDown())
	{	
		if(document.getElementById(formID) != null)			
			document.getElementById(formID).addEventListener("submit", gridEX_onsubmit, false);
		window.addEventListener("beforeunload", gridEX_onbeforeunload, false);
		window.addEventListener("resize", gridEX_onresize, false);			
	}		
	document.body.addEventListener("mousemove", body_onmousemove, false);
	document.body.addEventListener("select", body_onselectstart, false); 	
	htmlGridEX.addEventListener("blur", gridEX_onblur, false); 
	htmlGridEX.addEventListener("keydown", gridEX_onkeydown, false); 
	htmlGridEX.addEventListener("mousemove", gridEX_onmousemove, false);
	htmlGridEX.addEventListener("mouseup", gridEX_onmouseup, false);  
	htmlGridEX.addEventListener("select", gridEX_onselectstart, false);		
	if(!isDropDown())
		window.addEventListener("load", gridEX_onload, false);
		
	var gridEX = this;
	return this; 
}