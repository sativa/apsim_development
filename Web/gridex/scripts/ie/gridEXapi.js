//////////////////////////////////////////////////////////////////
// GridEX JavaScript IE 5.X API  (1.1.1009)
// Copyright by Janus Systems S.A. 
// 2002 - 2004
//////////////////////////////////////////////////////////////////
function GridEXGroupByBox(div, gridex)
{	
	var type = 0;
	var columns = null; 
	var gridEX = gridex;
	var htmlBox = div.all.tags("TABLE")[0];		
	this.column_onmousedown = column_onmousedown;
	this.column_onmouseup = column_onmouseup;
	this.column_onmousemove = column_onmousemove;
	this.getGridEX = getGridEX;
	this.getHeight = getHeight; 
	this.getHtmlBox = getHtmlBox;	
	this.DropColumn = DropColumn;
	this.HitTestColumns = HitTestColumns; 
	this.ShowColumnForDrop = ShowColumnForDrop;
	this.Unload = Unload; 
	function getGridEX() { return gridEX;  }	
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
		if(currcolumn != null && currcolumn.type == "header")
			return null;
			
		xlow = getPixelLeft(htmlBox) - getRequiredScrollLeft(null); 
		xhigh = xlow + htmlBox.offsetWidth;				
		ylow = getPixelTop(htmlBox) - getRequiredScrollTop(null);
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
						for(var i = 0; i < _colslength; i = i + 3)
						{						
							if(currheader.getGridEXTable().getID() == columns[i+2])
							{
								cell = columns[i+1];
								xcelllow = getPixelLeft(cell);								
								xcellhigh = xcelllow + cell.offsetWidth; 								
								ycelllow =  getPixelTop(cell); 
								ycellhigh = ycelllow + cell.offsetHeight;								
								if((x >= xcelllow && x <= xcellhigh) && (y >= ycelllow && y <= ycellhigh))
									return [cell, columns[i+2], 0]; 
							}
						}						
						_rowslength = htmlBox.rows.length; 						
						row = null; 
						table = null; 
						for(var index = 0; index < _rowslength; index++)
						{
							row = htmlBox.rows[index]; 
							if(currheader.getGridEXTable().getID() == row.id || (!IsTablePresent(currheader.getGridEXTable().getID()) && IsHierarchicalParent(row.id, currheader.getGridEXTable())))
							{						
								if(y >= (ylow + row.offsetTop) && y <= (ylow + row.offsetTop + row.offsetHeight))
								{								
									table = row.cells[0].childNodes[0];
									cell = table.cells[table.cells.length-1]; 
									if(x >= getPixelLeft(cell) + cell.offsetWidth)
									{	
										if(currheader.getGridEXTable().getID() == row.id)
											return [table.cells[table.cells.length-1], row.id, 1]; 
										else if(!IsTablePresent(currheader.getGridEXTable().getID()) && IsHierarchicalParent(row.id, currheader.getGridEXTable())) 
										{
											if(!groupsInTable(currheader.getGridEXTable().getID()))
												return [table.cells[table.cells.length-1], currheader.getGridEXTable().getID(), -1];
										}
									}
								}
							}
						}						
					}					
				}
				else if(type == 0)
					return [htmlBox.cells[htmlBox.cells.length-1], currheader.getGridEXTable().getID(), -1]; 
			}			
			else if(testmode == 2) 
			{	
				_colslength = columns.length; 
				for(var i = 0; i < _colslength; i = i + 3)
				{
					if(columns[i + 2] == currgrouptable)
					{
						cell = columns[i + 1]; 
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
				if(column.name == currcolumn.name)
					return; 
		
				xpos = getPixelLeft(column); 					
				ylow = getPixelTop(column); 
				yhigh = ylow + column.offsetHeight; 					
				var currpos = parseInt(currcolumn.name, 10);
				var newpos = parseInt(column.name, 10); 											
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
					groupnewpos = parseInt(column.name, 10);
				else if(x > xcellLow + (column.offsetWidth / 2) && x <= xcellLow + column.offsetWidth)
				{								
					xpos += column.offsetWidth; 
					groupnewpos = parseInt(column.name, 10) + 1;	
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
				var l = htmlBox.rows.length; 
				var row = null;
				for(var i = 0; i < l; i++)
				{
					row = htmlBox.rows[i]; 
					if(row.id == table) 
					{
						var _table = row.cells[0].childNodes[0]; 
						var cell = _table.cells[_table.cells.length-1];
						if(cell.name == null || cell.name == "")
							groupnewpos = 0; 
						else
							groupnewpos = parseInt(cell.name, 10) + 1;						

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
	function Unload()
	{		
		unloadArray(columns); 
		delete columns;	
		columns = null;
		if(htmlBox != null && htmlBox.type != null)
		{
			var l = htmlBox.rows.length; 
			var j = -1; 
			for(var i = 0; i < l; i++)
			{				
				var row = htmlBox.rows[i];
				var table = row.cells[0].childNodes[0]; 
				j = table.cells.length; 
				for(var g=0; g<j; g++)
				{
					var c = table.cells[g];
					if(c.type != null && parseInt(c.type, 10) == 1)
					{					
						c.detachEvent("onmousedown", gbbcolumn_onmousedown); 
						c.detachEvent("onmouseup", gbbcolumn_onmouseup); 
						c.detachEvent("onmousemove", gbbcolumn_onmousemove); 
						c.removeAttribute("groupbybox");																	
					}
				}
			}			
		}
		htmlBox.detachEvent("onclick", groupbyboxinfotext_onclick);
		htmlBox.parentElement.detachEvent("onclick", groupbybox_onclick);
		htmlBox = null;
		groupByBox = null;
		gridEX = null;		
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
				if(parseInt(currcolumn.name, 10) != currgrouppos)
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
					setGroupEventData(getGridEX().getID(), action, currgrouptable, currcolumn.name, "null", "null", "null"); 
				else
				{
					if(parseInt(currcolumn.name, 10) != currgrouppos)
						setGroupEventData(getGridEX().getID(), action, currgrouptable, currcolumn.name, groupnewpos, "null", "null"); 				
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
		if(window.event.button == 1)
		{			
			couldStartDrag = true;
			dragpoint = new Point(window.event.clientX, window.event.clientY); 
		}
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
		if(window.event.button == 1)
		{
			if(!columnDraging)
			{
				couldStartDrag = false; 
				dragpoint = null; 
				var _column = getColumnFromElement(window.event.srcElement); 
				var _input = document.getElementById(getGridEX().getID() + "_eventdata"); 
				if(_input == null)
					throw Error("unable to find event data"); 
				
				_input.value = getGroupTable(_column.id) + ":" + _column.name; 						
				getGridEX().DoPostBack(null, "GroupByBoxHeaderClick"); 
			}
		}
	}
	function groupbybox_onclick()
	{
		getGridEX().setHitTestArea(1); 
		getGridEX().FireEvent("Click", [getGridEX(), window.event.button, window.event.clientX, window.event.clientY]); 
	}
	function groupbyboxinfotext_onclick()
	{
		getGridEX().setHitTestArea(2); 
		getGridEX().FireEvent("Click", [getGridEX(), window.event.button, window.event.clientX, window.event.clientY]); 
		window.event.cancelBubble = true;
		window.event.returnValue = false;
		return false;
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
	function IsHierarchicalParent(parentID, table)
	{
		var t = table; 
		while(t.getParent() != null)
		{			
			if(t.getParent().getID() == parentID)
				return true; 
				
			t  = t.getParent(); 
		}
		return false; 
	}
	function IsTablePresent(tableID)
	{
		var l = htmlBox.rows.length; 
		for(var i = 0; i < l; i++)
		{
			if(htmlBox.rows[i].id == tableID)
				return true; 
		}
		return false;		
	}	
	if(htmlBox != null)
	{		
		if(htmlBox.type != null)
		{									
			var length = -1;
			columns = new Array(); 			
			var l = htmlBox.rows.length; 
			var j = -1; 
			for(var i = 0; i < l; i++)
			{				
				var row = htmlBox.rows[i];
				var table = row.cells[0].childNodes[0]; 
				j = table.cells.length; 
				for(var g=0; g<j; g++)
				{
					var c = table.cells[g];
					if(c.type != null && parseInt(c.type, 10) == 1)
					{
						length = columns.length; 
						columns[length] = c.id; 
						columns[length + 1] = c; 
						columns[length + 2] = row.id; 												
						c.attachEvent("onmousedown", gbbcolumn_onmousedown); 
						c.attachEvent("onmouseup", gbbcolumn_onmouseup); 
						c.attachEvent("onmousemove", gbbcolumn_onmousemove); 
						c.setAttribute("groupbybox", this);																		
					}
				}
			}			
			type = 1; 
		}
		else
		{
			type = 0; 
			htmlBox.attachEvent("onclick", groupbyboxinfotext_onclick ); 
		}			
		htmlBox.parentElement.attachEvent("onclick", groupbybox_onclick); 
	}	
	var groupByBox = this; 
	return this;
}
function GridEXSelectedItem(row)
{	
	var gridEXRow = row; 			
	this.getRow = getRow; 	
	this.Unload = Unload;
	function getRow() { return gridEXRow; }		
	function Unload() { gridEXRow = null; }
	return this; 
}
function GridEXSelectedItemCollection(gridex, selectedItems)
{	
	var gridEX = gridex; 	
	var arrSelectedItems = new Array();	
	if(selectedItems != null && selectedItems.length > 0)
	{	
		var row = null; 		
		for(var i=0; i<selectedItems.length; i++)
		{			
			row = document.getElementById(selectedItems[i]);
			if(row == null)
				throw Error("row '" + selectedItems[i] + "' is null or invalid for selected item"); 									
			
			arrSelectedItems[arrSelectedItems.length] = new GridEXSelectedItem(gridEX.RetrieveRow(row, getInnerItemRow(row) ,null)); 						
		}
		gridex.ReportRowsStatus();
	}	
	this.getSelectedItemInIndex = getSelectedItemInIndex;
	this.Clear = Clear; 
	this.Count = Count; 	
	this.IsRowSelected = IsRowSelected; 
	this.SelectRow = SelectRow;	
	this.SelectSingleRow = SelectSingleRow;
	this.Unload = Unload; 
	function getSelectedItemInIndex(index)
	{		
		if(index < 0 || index >= Count())
			throw Error("argument out of range");
			
		return arrSelectedItems[index]; 
	}
	function Count() { return arrSelectedItems.length; }
	function IsRowSelected(row)
	{
		if(arrSelectedItems.length == 0)
			return false; 
			
		var r= null; 
		var l = arrSelectedItems.length; 
		for(var i = 0; i < l; i++)
		{
			r = arrSelectedItems[i].getRow(); 
			if(r.getID() == row.getID() && r.getTable().getID() == row.getTable().getID())
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
				if(IsRowSelected(row))
					UnSelectRow(row, true);
				else
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
			if(window.event != null && window.event.type == "click")
			{
				if(gridEX.getHtmlGridEX().contains(window.event.srcElement))
					Clear(); 
			}
			Add(row); 
		}
		getGridEX().ReportRowsStatus(); 
	}
	function Unload()
	{
		for(var i=0;i<arrSelectedItems.length;i++)
		{
			var item = arrSelectedItems[i];
			item.Unload();
			delete item;
			item = null;			
		}
		delete arrSelectedItems;
		arrSelectedItems = null;
		gridEX = null;
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
		if(_tableID != null && getGridEX().getTables().getTableByID(_tableID).getUseColumnSets())
		{
			if(_innerItemRow == null)
				_innerItemRow = row;
			if(_innerItemRow.type == null || _innerItemRow.type == "3" || _innerItemRow.type == "4" || _innerItemRow.type == "9")
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
			
		if(IsRowSelected(row))
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
	function UnSelectRow(row, remove)
	{		
		var innerRow = row.getInnerRow();
		innerRow.className = getClassName(row);			
		var innerPreviewRow = row.getPreviewInnerRow(); 
		if(innerPreviewRow != null)
			innerPreviewRow.className = getPreviewClassName(row); 
			
		if(remove != null && remove == true)
		{
			var rowToRemove = -1; 
			var s = arrSelectedItems.length; 
			for(var i=0;i<arrSelectedItems.length&&rowToRemove==-1;i++)
			{
				if(arrSelectedItems[i].getRow() == row)
					rowToRemove = i;				
			}
			for(var i=rowToRemove;i<s-1;i++)
			{
				if(i != s)
					arrSelectedItems[i] = arrSelectedItems[i+1];								
			}
			arrSelectedItems.length = s-1;
		}
	}
	function Clear()
	{		
		for(var i = 0; i < arrSelectedItems.length; i++)
			UnSelectRow(arrSelectedItems[i].getRow());
			
		arrSelectedItems.length = 0; 
	}					
	return this; 
}
function GridEXColumnHeaders(gridEXTable, htmlRow, htmlTable, headerType, headerIndex, isRoot)
{		
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
	this.Unload = Unload; 
	this.UpdateColumnSize = UpdateColumnSize;	
	this.column_onmousedown = column_onmousedown;
	this.column_onmousemove = column_onmousemove;	
	this.column_onmouseover = column_onmouseover;
	this.column_onmouseup = column_onmouseup;
	this.column_onclick = column_onclick; 
	this.column_ondblclick = column_ondblclick;
	this.column_oncontextmenu = column_oncontextmenu;	
	var cellsresize = 0; 	
	function getColumnSets() { return columnsets; }	
	function getGridEXTable() { return gridEXTable; }	
	function getGridEX() { return gridEXTable.getGridEX(); }	
	function getHtmlHeader() { return htmlTable; }
	function getHtmlColumnInColumnSetById(id)
	{
		var c = null; 
		var cs = null; 
		var j = columnsets.getCount(); 
		for(var i = 0; i < j; i++)
		{				
			cs = columnsets.getColumnSetInIndex(i); 
			c = cs.getHtmlColumnByID(id); 
			if(c != null)
				return [c,i];
		}
		throw Error("argument out of range"); 
	}
	function getHtmlColumnById(id)
	{		
		if(headerType == 1) 
		{			
			var r = htmlTable.rows(0); 
			var c = null;
			var l = r.cells.length; 
			for(var i = 0; i < l; i++)
			{
				c = r.cells[i]; 
				if(c.id == id) 
					return c; 
			}			
			throw Error("argument out of range"); 
		}
		else 
		{
			var c = getHtmlColumnInColumnSetById(id);
			if(c != null && c.length == 2)
				return c[0];
				
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
		_cell.style.pixelWidth = (colY.offsetWidth + getGridEXTable().getHeaderWidth() - (getPaddingLeft(colY) + getPaddingRight(colY) + getBorderWidth(colY) + getSortWidth(colY))); 
		if(colY.getAttribute("pec") != null && colY.type != "ch")
			_cell.style.pixelWidth += 18;		
		
		_cell = colX.childNodes[0]; 
		_cell.style.pixelWidth = (colX.offsetWidth - getGridEXTable().getHeaderWidth() - (getPaddingLeft(colX) + getPaddingRight(colX) + getBorderWidth(colX) + getSortWidth(colX))); 
		if(colX.getAttribute("pec") != null && colX.type != "ch")
			_cell.style.pixelWidth += 18;
			
		colY.type = "ch";
		if(colX.getAttribute("pec") != null)
			colY.setAttribute("pec", colX.getAttribute("pec")); 
		
		colX.type = null;
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
		var length = _htmlTable.cells.length;
		var cellswidth = new Array();
		for(var i = 0; i < length; i++)
		{
			cell = _htmlTable.cells[i];
			if(cell.type != "rh" && cell.allowsize == null)							
				cellswidth[cellswidth.length] = cell.offsetWidth; 
		}
		var igcell = 0;
		var diff = 0;
		var cellsize = 0;
		for(var icell = 0; icell < length; icell++)
		{	
			cell = _htmlTable.cells[icell];
			if(cell.type != "rh" && cell.allowsize == null) 
			{
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
				cellsize = cellswidth[igcell];				
				_cell = htmlTable.cells[icell]; 
				if(cell.type == "ch" && _cell.type != "ch")
				{					
					htmlTable.getElementsByTagName("COL")[_cell.cellIndex].width = (cellsize - getGridEXTable().getHeaderWidth() - diff) + "px";
					if(_cell.getAttribute("pec") != null && _cell.type != "ch")
						_cell.childNodes[0].style.pixelWidth += 18; 
				}
				else 
				{
					htmlTable.getElementsByTagName("COL")[_cell.cellIndex].width = (cellsize) + "px";
					_cell.childNodes[0].style.pixelWidth = cellsize - diff;										
				}				
				igcell++; 
			}
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
			for(var i = 0; i < _headersLength && _header == null; i++)
			{
				_tmpHeader = _headers[i];
				if(_tmpHeader.getIndex() != getIndex() && _tmpHeader.getIsVisible()) 
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
				if(cell.childNodes.length == 1 && cell.childNodes[0].childNodes.length == 1)
				{					
					var element = cell.childNodes[0].childNodes[0]; 
					if(element.nodeType == 1 && element.tagName == "INPUT" && element.type == "checkbox")
						element.checked = checked; 
				}
			}
		}		
		getGridEXTable().setSelectorStatus(checked);		
		if(updateStatus)
		{		
			var _data = ""; 
			var _table = null; 
			var j = getGridEX().getTables().Count(); 
			for(var i = 0; i < j; i++)
			{			
				_table = getGridEX().getTables().getTableInIndex(i); 
				if(_table.getSelectorStatus() || _table == getGridEXTable())
				{
					if(_data.length > 0)
						_data += "|"; 
						
					_data += _table.getID(); 
					_data += ",";
					_data += checked ? "1" : "0"; 
				}
			}			
			var _input = window.document.getElementById(getGridEX().getID() + "_selectordata"); 
			if(_input != null)
				_input.value = _data;
		}
	}
	function DropColumn(column)
	{
		if(column == null) 
			throw Error("column for drop is null");
		
		if(columndragMode == 3)
		{											
			var cancel = getGridEX().FireEvent("GroupsChanging", [getGridEXTable().getColumns().getGridEXColumnByClientID(currcolumn.id), 2]);
			if(cancel == null || !cancel)
			{
				var _gridexID = getGridEX().getID(); 
				setGroupEventData(getGridEX().getID(), 2, currgrouptable, currcolumn.name, "null", "null", columnfordrop.pos);						
				getGridEX().DoPostBack(null, "GroupsChanging"); 
			}
			endColumnDrag();
		}
		else
		{	
			if(columnfordrop == null)
				return; 
		
			var direction = 1;			
			var lowpos = parseInt(column.pos, 10);
			var highpos = parseInt(columnfordrop.pos, 10); 			
			var _colX = getColumnInPosition(lowpos);
			var _colY = getColumnInPosition(highpos);			
			var columnX = getGridEXTable().getColumns().getGridEXColumnByClientID(_colX.id); 
			var columnY = getGridEXTable().getColumns().getGridEXColumnByClientID(_colY.id);			
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
				
			var arraypos = null; 
			if(getGridEXTable().tabCellsOrder != null)
				arraypos = new Array(getGridEXTable().getColumns().Count()); 
			var adjustType = -1; 
			var inallheaders = getGridEXTable().getHeaders().length > 1;	
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
					_colX = getColumnInPosition(lowpos, false); 
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
						if(_colX.type != null && _colX.type == "ch")
						{
							adjustType = 1; 
							swapColumnHeader(_colX, _colY);
						}
						else if(_colY.type != null && _colY.type == "ch")
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
						if(arraypos != null)
						{
							if(columnX.getSelectable())
								arraypos[columnX.position] = _colX.id;
							if(columnY.getSelectable())
								arraypos[columnY.position] = _colY.id;						
						}
						_colY.pos = lowpos; 
						_colX.pos = lowpos + direction;
						SwapItems(_colX, _headerColX.id, _colY, _headerColY.id);
						if(inallheaders)
							SwapColumnInHeaders(_colX.id, lowpos + direction, _colY.id, lowpos); 
										
						lowpos += (direction*_iposoffset); 
					}
				}
			}	while(lowpos != highpos)
			if(arraypos != null)
			{
				var newtaborder = new Array();
				for(var i=0;i<arraypos.length;i++)
				{
					if(arraypos[i] == null)
					{
						for(var j=0;j<getGridEXTable().tabCellsOrder.length;j=j+2)
						{
							if(getGridEXTable().tabCellsOrder[j] == i)
							{
								newtaborder[newtaborder.length] = i;
								newtaborder[newtaborder.length] = getGridEXTable().tabCellsOrder[j+1];
							}
						}
					}
					else
					{
						newtaborder[newtaborder.length] = i;
						newtaborder[newtaborder.length] = arraypos[i];
					}
				}
				getGridEXTable().tabCellsOrder = newtaborder; 
			}
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
		var isrtl = (_gridex.getHtmlGridEX().getAttribute("rtl") == "1");
		xlow = getPixelLeft(htmlTable) + getRequiredScrollLeft(_gridex);	
		ylow = getPixelTop(htmlTable);
		if(!getIsRoot()) 
			ylow -= getAdjustPixelTop(_gridex); 
		else
			ylow -= getAdjustPixelTop(_gridex, false, true);		
		if(!getIsRoot() && !isrtl)
			x += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
		else if(!getIsRoot())
		{
			x -= (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth) - getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
			if(getGridEXTable().getHtmlItemsTable().offsetLeft >= 0)
					x -= (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);
		}
		if(isrtl)
		{
			x -= fixRightToLeftScroll();
			x -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft);
		}
		xhigh = xlow + htmlTable.offsetWidth;					
		yhigh = ylow + htmlTable.offsetHeight; 		
		if((x >= xlow && x <= xhigh) && (y >= ylow && y <= yhigh))
		{			
			var xcelllow = null;
			var xcellhigh = null;
			var ycelllow = null; 
			var ycellhigh = null; 			
			var _length = htmlTable.cells.length;
			if(!isrtl)
			{
				for(var i = 0; i < _length; i++)
				{
					cell = htmlTable.cells[i];
					if(cell.type != "rh")
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
			else
			{
				for(var i=_length-1;i>=0;i--)
				{	
					cell = htmlTable.cells[i];
					if(cell.type != "rh")
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
		var _isrtl = (_gridex.getHtmlGridEX().getAttribute("rtl") == "1");
		offsetleft = 	getPixelLeft(htmlTable) + getRequiredScrollLeft(_gridex) + column.offsetLeft;		
		if(!getIsRoot() && !_isrtl)
			x += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;	
		else if(!getIsRoot())
		{
			x -= (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth) - getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
			if(getGridEXTable().getHtmlItemsTable().offsetLeft >= 0)
					x -= (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);
		}
		if(_isrtl)
		{
			x -= fixRightToLeftScroll(); 
			x -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft);
		}			
		if(x >= offsetleft && x <= offsetleft + (column.offsetWidth / 2))
		{				
			if(column.cellIndex - 1 >= 0 && htmlTable.cells[column.cellIndex-1].type != "rh") 	
			{					
				if(columndragMode != 3)
				{					
					if(_isrtl && column.cellIndex + 1 < htmlTable.cells.length && currcolumn.id == htmlTable.cells[column.cellIndex+1].id)
						return;
					else if(!_isrtl && currcolumn.id == htmlTable.cells[column.cellIndex-1].id)
						return;	
				}				
				if(getIsRoot())
					xpos = getPixelLeft(column);
				else
				{
					xpos = getPixelLeft(column)- 1; 
					if(!_isrtl)
						xpos -= getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
					else
					{
						xpos += (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
						if(getGridEXTable().getHtmlItemsTable().offsetLeft >= 0)
							xpos += (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);						
					}						
				}
				ylow = getPixelTop(column);
				if(!getIsRoot())
					ylow -= getAdjustPixelTop(_gridex, true);
				yhigh = ylow + column.offsetHeight; 				
				drawUpArrow(xpos, ylow);
				drawDownArrow(xpos, yhigh);								
				columnfordrop = column;				
			}
			else if((column.cellIndex == 0 && column.type == "ch" && getIsRoot()) || (column.cellIndex == 0 && column.type != "rh" && !getIsRoot()))
			{
				if(columndragMode != 3 && column.id == currcolumn.id)
					return; 
								
				if(getIsRoot())
					xpos = getPixelLeft(column);
				else
				{
					xpos = getPixelLeft(column) - 1;
					if(!_isrtl)
						xpos -= getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft; 			
					else
					{
						xpos += (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
						if(getGridEXTable().getHtmlItemsTable().offsetLeft >= 0)
							xpos += (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);
					}
				}				
				ylow = getPixelTop(column);
				if(!getIsRoot())
					ylow -= getAdjustPixelTop(_gridex, true);				
				yhigh = ylow + column.offsetHeight; 				
				drawUpArrow(xpos, ylow);
				drawDownArrow(xpos, yhigh);								
				columnfordrop = column;
			}			
		}
		else if(x > offsetleft + (column.offsetWidth / 2) && x <= offsetleft + column.offsetWidth)		
		{	
			if(column.cellIndex + 1 < htmlTable.cells.length) 
			{	
				if(columndragMode != 3)
				{
					if(_isrtl && column.cellIndex-1 >= 0 && currcolumn.id == htmlTable.cells[column.cellIndex-1].id)
						return;
					else if(!_isrtl && currcolumn.id == htmlTable.cells[column.cellIndex+1].id)
						return;
				}					
				if(getIsRoot())
					xpos = getPixelLeft(column) + column.offsetWidth;
				else
				{
					xpos = getPixelLeft(column) + column.offsetWidth; 
					if(!_isrtl)
						xpos -= getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
					else
					{
						xpos += (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
						if(getGridEXTable().getHtmlItemsTable().offsetLeft >= 0)
							xpos += (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);						
					}
				}
				ylow = getPixelTop(column);
				if(!getIsRoot()) 
					ylow -= getAdjustPixelTop(_gridex, true); 				
				yhigh = ylow + column.offsetHeight;				
				drawUpArrow(xpos, ylow);
				drawDownArrow(xpos, yhigh); 				
				if(currcolumn.cellIndex < column.cellIndex)
					columnfordrop = column;
				else
					columnfordrop = htmlTable.cells[column.cellIndex+1];
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
		for(var iheader = 0; iheader < _arrheadersLength; iheader++)
		{			
			if(headerindex != iheader)
			{
				_htmlheader = arrheaders[iheader].getHtmlHeader();
				_cols = _htmlheader.getElementsByTagName("COL"); 
				_cell = _htmlheader.cells[column.cellIndex]; 
				if(column.type != null && column.type == "ch" && _cell.type != "ch")
				{					
					_cellWidth = cellWidth - getGridEXTable().getHeaderWidth(); 
					_colWidth = colWidth - getGridEXTable().getHeaderWidth(); 
					if(_cell.getAttribute("pec") != null && _cell.type != "ch")
					{
						_cellWidth += 18;
						_colWidth += 18;
					}						
					_cell.childNodes[0].style.pixelWidth = _cellWidth; 
					_cols[_cell.cellIndex].width = _colWidth + "px";
				}
				else if(column.type == null && _cell.type != null && _cell.type == "ch")				
				{				
					_cellWidth = cellWidth + getGridEXTable().getHeaderWidth(); 		
					_colWidth = colWidth + getGridEXTable().getHeaderWidth(); 		
					if(_cell.getAttribute("pec") != null && _cell.type != "ch")
					{
						_cellWidth += 18;
						_colWidth += 18; 
					}						
					_cell.childNodes[0].style.pixelWidth = _cellWidth; 
					_cols[_cell.cellIndex].width = _colWidth + "px"; 
				}
				else
				{
					_cell.childNodes[0].style.pixelWidth = cellWidth; 
					_cols[_cell.cellIndex].width = colWidth + "px"; 
				}
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
			column.childNodes[0].style.pixelWidth = _width; 
			_cols[column.cellIndex].width = width + "px"; 
			column.style.pixelWidth = width; 
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
	function ResizeColumnWidth(column, posX)	
	{			
		var _gridex = getGridEXTable().getGridEX(); 		
		var _rtl = _gridex.getHtmlGridEX().getAttribute("rtl") == "1";		
		var cellindex = column.cellIndex;		
		posX += getScrollLeft(_gridex); 	
		var offsetwidth;		
		if(_rtl)
		{
			posX -= fixRightToLeftScroll();	
			posX -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft); 		
			offsetwidth = getPixelLeft(column) - posX; 
		}
		else
			offsetwidth = posX - (column.offsetWidth + getPixelLeft(column));
		if(!getIsRoot() && !_rtl)
			offsetwidth += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
		else if(!getIsRoot())
		{
			offsetwidth += Math.abs((getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft);
			offsetwidth += (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);
		}		
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
		if(cellindex + 1 < htmlTable.cells.length)				
		{					
			if(getGridEX().getColumnAutoResize())
			{
				oldremainwidth = GetCellsWidth(cellindex + 1, htmlTable.cells.length - 1);
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
			column.childNodes[0].style.pixelWidth = _width;
			_cols[column.cellIndex].width = newwidth + "px"; 
			column.style.pixelWidth = newwidth;
			if(inallheaders)				
				ResizeColumnInHeaders(column, _width, newwidth); 
				
			if(getGridEX().getColumnAutoResize())
			{							
				ResizeColumns(cellindex + 1, htmlTable.cells.length - 1, oldremainwidth, newremainwidth, _cols);						 	
				if(inallheaders)
					FixAutoSizeWidth(oldwidth, _headers, _cols); 
				else
					FixAutoSizeWidth(oldwidth, null, _cols); 
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
			column.childNodes[0].style.pixelWidth = _width;
			_cols[column.cellIndex].width = newwidth + "px"; 
			column.style.pixelWidth = newwidth; 
			if(inallheaders)
				ResizeColumnInHeaders(column, _width, newwidth); 
			
			if(getGridEX().getColumnAutoResize())
			{
				ResizeColumns(0, cellindex - 1, oldremainwidth, newremainwidth, _cols); 
				if(inallheaders)
					FixAutoSizeWidth(oldwidth, getGridEXTable().getHeaders(), _cols); 
				else
					FixAutoSizeWidth(oldwidth, null, _cols);
			}			
			_colswidth = new Array(); 	
			updateColumnsDefinition(_colswidth);				
			AutoSizeItems(_colswidth);						
		}
		resetRootTableScroll(getGridEX().getRootTable()); 			
	}
	function Unload()
	{
		if(columnsets != null)
			columnsets.Unload();		
		delete columnsets;
		if(htmlTable != null)
		{
			htmlTable.detachEvent("onselectstart", header_onselectstart); 
			htmlTable.detachEvent("onmousedown", header_onmousedown);
			var row = htmlTable.rows[0];
			var c = null; 
			var l = row.cells.length; 
			for(var i = 0; i < l; i++)
			{
				c = row.cells[i];
				if(c.type != "rh")
				{	
					c.detachEvent("onmousedown", hcolumn_onmousedown);
					c.detachEvent("onmousemove", hcolumn_onmousemove);
					c.detachEvent("onmouseover", hcolumn_onmouseover);				
					c.detachEvent("onmouseup", hcolumn_onmouseup);
					c.detachEvent("onclick", hcolumn_onclick); 
					c.detachEvent("ondblclick", hcolumn_ondblclick);
					c.detachEvent("oncontextmenu", hcolumn_oncontextmenu); 
					c.removeAttribute("header");							
				}						
			}
		}
		htmlTable = null;
		htmlRootRow = null;
		columnsets = null;
		gridEXTable = null;
		gridEXColumnHeaders = null;		
	}
	function getFixedHierarchyWidth()
	{
		var width = 0; 
		var hi = htmlTable.parentElement.parentElement.cellIndex; 
		var t = htmlTable.parentElement.parentElement.offsetParent; 
		var l = t.cells.length;
		for(var i = 0; i < l; i++)
		{
			if(i != hi)
				width += t.cells[i].offsetWidth; 
		}		
		return width; 
	}			
	function updateColumnsDefinition(columnsWidth)
	{		
		var _field = document.getElementById(getGridEXTable().getID() + "_cols");
		if(_field == null)
			throw Error("input field for columns definition is null");
			
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var _length = htmlTable.cells.length;
		var _width = null; 
		var cell = null;				
		for(var i = 0; i < _length; i++)
		{			
			cell = htmlTable.cells[i];
			if(cell.id != null && cell.pos != null)
			{				
				if(cell.allowsize == null && columnsWidth != null)
				{
					_width = cell.offsetWidth; 
					if(cell.type == "ch")
						_width -= getGridEXTable().getHeaderWidth();
						
					updateColumnDefinitionInField(_field, cell.id, parseInt(cell.pos, 10), _width);
				}
				else if(columnsWidth == null)
					updateColumnDefinitionInField(_field, cell.id, parseInt(cell.pos, 10), -1);					
			}				
			if(columnsWidth != null)
			{
				if(cell.type != "rh")				
				{				
					_width = cell.offsetWidth; 
					if(cell.type == "ch")
						_width -= getGridEXTable().getHeaderWidth();						
					
					columnsWidth[columnsWidth.length] = _cols[i].id;
					columnsWidth[columnsWidth.length] = _width; 										
				}
			}
		}				
	}
	function AutoSizeItems(columnsWidth)
	{	
		var _itemsTables = document.getElementsByName(getGridEXTable().getID() + "_i"); 
		var _itemsCols = null; 
		var _itemsColsLength = -1; 
		var _colwidthLength = columnsWidth.length; 		
		var _customApplied = false;
		var _itemCol = null; 
		var _itemsTablesLength = _itemsTables.length; 
		var _width = -1; 
		if(_itemsTablesLength > 0)
		{			
			for(var _item = 0; _item < _itemsTablesLength; _item++)
			{											
				_itemsCols = _itemsTables[_item].getElementsByTagName("COL"); 				
				for(var _icol = 0;  _icol < _colwidthLength; _icol = _icol + 2)
				{					
					_itemCol = _itemsCols.item(columnsWidth[_icol]); 
					if(_itemCol.type != "space")
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
			}			
		}
		else if(getGridEXTable().getParent() == null)
		{			
			var _htmlitemstable = getGridEXTable().getHtmlItemsTable(); 
			if(_htmlitemstable.getAttribute("empty") != null)
				_htmlitemstable.style.pixelWidth = getGridEXTable().getWidth(); 
		}
		var _newdiv = document.getElementsByName("nrsep" + getGridEXTable().getID()); 
		if(_newdiv != null)
		{
			for(var i=0; i< _newdiv.length; i++)
				_newdiv[i].style.pixelWidth = getGridEXTable().getWidth(); 
		}
		var thdiv = document.getElementsByName("th" + getGridEXTable().getID());
		if(thdiv != null)
		{
			for(var i=0;i<thdiv.length;i++)	
			{
				var thwidth = getGridEXTable().getWidth();
				if(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset") != null)
					thwidth -= parseInt(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset"), 10);					
				thdiv[i].getElementsByTagName("COL")[0].width = thwidth + "px";
			}				
		}
		if(getGridEX().getResizeGroups())
		{
			var _offset = 0; 
			var _tablewidth = getGridEXTable().getWidth(); 			
			_itemsTables = document.getElementsByName("group" + getGridEXTable().getID()); 
			_itemsTablesLength = _itemsTables.length; 
			if(_itemsTablesLength > 0)
			{
				for(var i = 0; i < _itemsTablesLength; i++)
				{										
					_itemsCols = _itemsTables[i].getElementsByTagName("COL"); 
					_itemCol = _itemsCols[0]; 
					_offset = _itemCol.getAttribute("offset"); 
					_itemCol.width = (_tablewidth - _offset) + "px"; 
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
		if(newwidth <= 0)
			return; 
			
		var cell = null; 
		var _cell = null;		
		var length = htmlTable.cells.length; 
		var cellswidth = new Array();
		for(var i = 0; i < length; i++)
		{
			cell = htmlTable.cells[i];
			if(cell.type != "rh" && cell.allowsize == null)				
				cellswidth[cellswidth.length] = getPixelColWidth(_cols[i].width); 
		}
		var igcell = 0;
		var diff = 0;
		var cellsize = 0;		
		var _headersLength = -1; 
		var _table = null; 
		for(var icell = 0; icell < length; icell++)
		{	
			cell = htmlTable.cells[icell];
			if(cell.type != "rh" && cell.allowsize == null) 
			{
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
				cellsize = Math.round((cellswidth[igcell] * newwidth) / oldwidth);
				if(cellsize - diff > 0 || (diff - getSortWidth(cell) > 0))
				{
					if(cellsize - diff > 0)
					{
						cell.childNodes[0].style.pixelWidth = cellsize - diff;																				
						_cols[icell].width = cellsize  + "px"; 				
					}
					else
					{
						var _usewidth = 0; 
						if(cellsize - diff ==  0 || cellsize <= 0) 
							_usewidth = diff - getSortWidth(cell);
						else							
							_usewidth = cellsize; 
						
						cell.childNodes[0].style.pixelWidth = _usewidth;
						_cols[icell].width =  _usewidth + "px";
					}
					if(headers != null && headers.length > 1)
					{
						_headersLength = headers.length; 
						_table = null; 					
						for(var iheader = 1; iheader < _headersLength; iheader++)
						{
							if(headers[iheader].getIsVisible())
							{
								_table = headers[iheader].getHtmlHeader();
								_childcols = _table.getElementsByTagName("COL"); 
								_cell = _table.cells[icell];
								if(cell.type == "ch" && _cell.type != "ch")
								{
									var _usewidth = 0; 
									if(cellsize - diff > 0)
										_usewidth = cellsize - diff;
									else if(cellsize - diff == 0 || cellsize <= 0)
										_usewidth = diff - getSortWidth(cell);
									else
										_usewidth = cellsize; 
										
									_childwidth = _usewidth - getGridEXTable().getHeaderWidth(); 
									if(_cell.pec != null && _cell.type != "ch")
										_childwidth += 18;
									_cell.childNodes[0].style.pixelWidth = _childwidth; 
									_childcols[_cell.cellIndex].width = (_childwidth + diff) + "px"; 
								}
								else 
								{
									if(cellsize - diff > 0)
									{
										_cell.childNodes[0].style.pixelWidth = cellsize - diff;
										_childcols[_cell.cellIndex].width = cellsize + "px"; 
									}
									else
									{
										var _usewidth = 0;
										 if(cellsize - diff == 0 || cellsize <= 0)
											_usewidth = diff - getSortWidth(cell);
										else
											_usewidth = cellsize; 
											
										_cell.childNodes[0].style.pixelWidth = _usewidth; 
										_childcols[_cell.cellIndex].width = (_usewidth + diff) + "px"; 
									}
								}
							}
						}
					}				
				}				
				igcell++; 
			}
		}
		FixAutoSizeWidth(width, headers, _cols);
		var columnsWidth = new Array(); 
		updateColumnsDefinition(columnsWidth); 
		AutoSizeItems(columnsWidth);		
	}			
	function FixAutoSizeWidth(newwidth, headers, cols)
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
				if(cell.type != "rh" && cell.allowsize == null)
				{	
					_childcell = cell.childNodes[0];
					if(cell.type == "ch")
						_lowwidth = getGridEXTable().getHeaderWidth(); 												
					else
					{						
						if(cell.pec != null)
							_lowwidth = 18; 
						else
							_lowwidth = 0; 												
					}								
					_style = _childcell.style;					
					if(_style.pixelWidth == 0)
					{
						if(_childcell.currentStyle.width != "")
							_width = getPixelWidth(_childcell.currentStyle.width); 
					}
					else
						_width = _style.pixelWidth; 
					
					if(_width + offset > _lowwidth)
					{						
						_style.pixelWidth = (_width + offset);
						_col = cols[index]; 
						_col.width = (getPixelColWidth(_col.width) + offset) + "px";
						if(headers != null)
						{														
							for(var iheader = 0; iheader < _length; iheader++)
							{
								if(iheader != headerindex)
								{
									_table = headers[iheader].getHtmlHeader();
									_childcols = _table.getElementsByTagName("COL"); 
									_childcell = _table.cells[index].childNodes[0];
									if(_childcell.style.pixelWidth == 0)
									{
										if(_childcell.currentStyle.width != "")
											_childwidth = getPixelWidth(_childcell.currentStyle.width);
									}
									else
										_childwidth = _childcell.style.pixelWidth;
										
									_childwidth += offset; 
									_childcell.style.pixelWidth = _childwidth;
									_childcol = _childcols[index]; 
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
	}	
	function getColumnInPosition(position, throwError)
	{
		var l = htmlTable.cells.length; 
		var c = null; 		
		for(var i = 0; i < l; i++)
		{
			c = htmlTable.cells[i];
			if(c.pos != null)
			{
				if(parseInt(c.pos, 10) == position)
					return c; 
			}
		}		
		if(throwError == null || throwError)
			throw Error("argument out of range"); 
		else
			return null; 		
	}	
	function GetCellsWidth(lowcell, highcell)
	{
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var width = 0; 
		var cell = null; 
		var _col = null; 
		while(lowcell <= highcell)
		{
			cell = htmlTable.cells[lowcell];
			if(cell.type != "rh" && cell.allowsize == null)
			{
				_col = _cols[lowcell]; 
				if(_col.width.indexOf("px") == -1)
					width += parseInt(_col.width, 10); 
				else
					width += getPixelWidth(_col.width); 
			}				
			lowcell++;
		}
		return width; 
	}	
	function ResizeColumnsExcept(columnIndex, oldwidth, newwidth, cols)
	{
		var igcell = 0; 		
		var cell = null; 
		var cellwidth = 0; 		
		var cellswidth = new Array(); 
		var diff = 0; 
		var l = htmlTable.cells.length; 
		for(var i = 0; i < l; i++)
		{
			if(i != columnIndex)
			{
				 cell = htmlTable.cells[i]; 
				 if(cell.type != "rh" && cell.allowsize == null)
					cellswidth[cellswidth.length] = cell.offsetWidth; 	
			}
		}
		var inallheaders = (getGridEXTable().getHeaders().length > 1);
		for(var i = 0; i < l; i++)
		{
			if(i != columnIndex)
			{
				cell = htmlTable.cells[i]; 
				if(cell.type != "rh" && cell.allowsize == null)
				{
					cellwidth = Math.round((cellswidth[igcell] * newwidth) / oldwidth);	
					diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);				
					cell.childNodes[0].style.pixelWidth = (cellwidth - diff); 
					cols[i].width =  cellwidth + "px";					
					if(inallheaders)
						ResizeColumnInHeaders(cell, cellwidth - diff, cellwidth);
					
					igcell++; 
				}				
			}
		}
	}
	function ResizeColumns(lowcell, highcell, oldwidth, newwidth, cols)
	{			
		var cell = null; 
		var cellswidth = new Array(); 
		for(var i = lowcell; i <= highcell; i++)
		{			
			cell = htmlTable.cells[i];
			if(cell.type != "rh" && cell.allowsize == null) 
				cellswidth[cellswidth.length] = cell.offsetWidth; 				
		}	
		var inallheaders = (getGridEXTable().getHeaders().length > 1);
		var igcell = 0; 
		var cellwidth = 0;
		var diff = 0; 
		while(lowcell <= highcell)
		{			
			cell = htmlTable.cells[lowcell];
			if(cell.type != "rh" && cell.allowsize == null) 
			{
				cellwidth = Math.round((cellswidth[igcell] * newwidth) / oldwidth);	
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);				
				cell.childNodes[0].style.pixelWidth = (cellwidth - diff); 
				cols[lowcell].width =  cellwidth + "px"; 
				igcell++; 				
				if(inallheaders)
					ResizeColumnInHeaders(cell, cellwidth - diff, cellwidth);									
			}
			lowcell++; 
		}
	}
	function UpdateColumnSize(colID, colWidth)
	{
		var inallheaders = getGridEXTable().getHeaders().length > 1; 
		var column = htmlTable.all(colID);
		column.childNodes[0].style.pixelWidth = colWidth; 
		if(inallheaders)
			ResizeColumnInHeaders(column, colWidth); 						
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
		var h = -1; 
		var _itemtable = null; 
		var _itemstables = document.getElementsByName(getGridEXTable().getID() + "_i"); 
		var l = _itemstables.length; 
		var cellsX = null; 
		var cellsY = null; 
		var xID = columnX.id;
		var yID = columnY.id;			 
		if(l > 0)
		{		
			for(var i = 0; i < l; i++)
			{
				_itemtable = _itemstables[i];
				_cols = _itemtable.getElementsByTagName("COL"); 				
				_coly = _cols.item(colYID); 
				_colx = _cols.item(colXID);
				if(_coly.getAttribute("pec") != null) 
					swapItemHeaders(_coly, _colx); 
				else if(_colx.getAttribute("pec") != null)
					swapItemHeaders(_colx, _coly);
							
				_coly.swapNode(_colx);												
			}
		}		
		_itemtable = getGridEXTable().getHtmlItemsTable(); 
		if(_itemtable.getAttribute("empty") != null)
			return; 
			
		cellsX = document.all(xID + "_L"); 
		cellsY = document.all(yID + "_L");
		if(cellsX.length > 0)
		{
			h = cellsX.length; 
			for(var i = 0; i < h; i++)
				cellsY[i].swapNode(cellsX[i]); 
		}
		else
			cellsY.swapNode(cellsX);
	}	
	function SwapColumnInHeaders(xID, xPos,  yID, yPos)
	{
		var arrheaders = getGridEXTable().getHeaders(); 		
		var colX = null;
		var colY = null;
		var _headerColY = null;
		var _headerColX = null; 				
		var _cols = null; 		
		var _htmlheader = null; 
		var _length = arrheaders.length;
		for(var iheader = 0; iheader < _length; iheader++)
		{
			if(headerindex != iheader)
			{
				_htmlheader = arrheaders[iheader].getHtmlHeader();
				_cols = _htmlheader.getElementsByTagName("COL");
				colX = _htmlheader.all(xID);
				colY = _htmlheader.all(yID);
				_headerColY = _cols[colY.cellIndex]; 
				_headerColX = _cols[colX.cellIndex];
				_headerColX.swapNode(_headerColY);
				if(colX.type == "ch")
				{
					_headerColY.width = getPixelColWidth(_headerColY.width) + getGridEXTable().getHeaderWidth(); 
					_headerColX.width = getPixelColWidth(_headerColX.width) - getGridEXTable().getHeaderWidth(); 
				}
				else if(colY.type == "ch")
				{
					_headerColX.width = getPixelColWidth(_headerColX.width) + getGridEXTable().getHeaderWidth(); 
					_headerColY.width = getPixelColWidth(_headerColY.width) - getGridEXTable().getHeaderWidth(); 
				}
				if(colX.getAttribute("ec") != null && colX.type != "ch")
				{
					_headerColY.width = getPixelColWidth(_headerColY.width) + 18;
					_headerColX.width = getPixelColWidth(_headerColX.width) - 18;
				}
				else if(colY.getAttribute("ec") != null && colY.type != "ch")
				{						
					_headerColX.width = getPixelColWidth(_headerColX.width) + 18;
					_headerColY.width = getPixelColWidth(_headerColX.width) - 18;
				}				
				if(colY.type != null && colY.type == "ch")
					swapColumnHeader(colY, colX);
				else if(colX.type != null && colX.type == "ch")
					swapColumnHeader(colX, colY);
					
				colX.swapNode(colY);
				colX.pos = xPos;
				colY.pos = yPos;				
			}
		}
	}		
	function getFixedWidth()
	{
		var l = htmlTable.cells.length; 
		var fixedwidth = 0;
		var cell = null; 
		for(var i = 0; i < l; i++)
		{
			cell = htmlTable.cells[i];
			if(cell.type == "rh" || cell.allowsize != null)
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
		if(column.allowsize == null)
		{			
			if(isInResizeArea(column, htmlTable, getGridEXTable()) && !columnDraging)
				column.style.cursor = cursorResize;			
		}		
	}	
	function column_onmousedown()
	{	
		if(window.event.button == 1)	
		{		
			var column = getColumnFromElement(window.event.srcElement);			
			if(column.id == null || column.id.length == 0)
				return;
			if(!columnResizing)
			{	
				var gridEXColumn = getGridEXTable().getColumns().getGridEXColumnByClientID(column.id);
				if(gridEXColumn.getAllowSize() || gridEXColumn.getAllowDrag()) 
				{
					if(column.style.cursor == cursorResize && gridEXColumn.getAllowSize())
					{
					     couldStartResize = true; 
					     if(getGridEXTable().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
					     {
							if(column.cellIndex - 1 >= 0)
								column = htmlTable.cells[column.cellIndex-1];
					     }					     
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
							dragpoint = new Point(window.event.clientX, window.event.clientY); 
						}						
					}
				}
			}			
			window.event.cancelBubble = true; 
		}
	}	
	function column_onmousemove()
	{	
		if(couldStartDrag && dragpoint != null)
		{		
			if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
			{
				var column = null;
				if(couldDragColumn != null)
					column = getColumnFromElement(couldDragColumn); 
				else
					column = getColumnFromElement(window.event.srcElement); 				
				startColumnDrag(column, gridEXColumnHeaders);
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
			if(column.allowsize == null)
			{				
				if(isInResizeArea(column, htmlTable, getGridEXTable()) && !columnDraging)
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
		if(eventButton == 1)
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
						var input = document.getElementById(getGridEX().getID() + "_eventdata"); 			
						if(input == null)
							throw Error("sort info field is null"); 				
					
						input.value = getGridEXTable().getID() + ":" + column.getClientID();  
						getGridEX().DoPostBack(null, "ColumnHeaderClick"); 
					}
				}
			}
			else if(columnResizing)
				endColumnResize(); 							
				
			if(gridEXTable.getGridEX().getThemedAreas() == 1)
			{
				ShowColumnUnPressed();
				currpressedcolumn = null;
				couldStartDrag = false;
				couldDragColumn = couldDragHeader = null;
				dragpoint = null; 
			}				
			if(tdColumn != null && tdColumn.style.cursor != cursorResize)
			{				
				window.event.cancelBubble = true;
				window.event.returnValue = false; 
			}	
		}
		getGridEXTable().getGridEX().setHitTestArea(5); 
		getGridEXTable().getGridEX().FireEvent("Click", [getGridEXTable().getGridEX(),eventButton, window.event.clientX, window.event.clientY]); 
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
			
			if(htmlColumn.type == "ch")
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
	function column_ondblclick()
	{	
		cancelColumnResize(); 		
		var column = getColumnFromElement(window.event.srcElement);
		if(column.style.cursor == cursorResize)
		{			
			var gridEXColumn = getGridEXTable().getColumns().getGridEXColumnByClientID(column.id); 
			ColumnAutoSize(gridEXColumn, column); 
		}
		getGridEXTable().getGridEX().setHitTestArea(5); 
		getGridEXTable().getGridEX().FireEvent("DoubleClick", [getGridEXTable().getGridEX(), window.event.clientX, window.event.clientY]); 
	}
	if(headerType == 1) 
	{			
		var r = htmlTable.rows[0];
		var c = null; 
		var l = r.cells.length; 
		for(var i = 0; i < l; i++)
		{
			c = r.cells[i];
			if(c.type != "rh")
			{	
				c.attachEvent("onmousedown", hcolumn_onmousedown);
				c.attachEvent("onmousemove", hcolumn_onmousemove);
				c.attachEvent("onmouseover", hcolumn_onmouseover);				
				c.attachEvent("onmouseup", hcolumn_onmouseup);
				c.attachEvent("onclick", hcolumn_onclick);
				c.attachEvent("ondblclick", hcolumn_ondblclick);
				c.attachEvent("oncontextmenu", hcolumn_oncontextmenu); 
				c.setAttribute("header", this);							
			}			
			if(c.type != "rh" && c.allowsize == null)
				cellsresize++;
		}		
	}	
	if(htmlTable != null)
	{				
		htmlTable.attachEvent("onselectstart", header_onselectstart); 
		htmlTable.attachEvent("onmousedown", header_onmousedown);
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
	function Count() { return childTables.length;}	
	function Add(childTable)
	{		
		if(getTableByID(childTable.getID()) == null)		
			childTables[childTables.length] = childTable;
	}	
	function getTableByID(id)
	{
		var c = null; 
		var l = childTables.length; 
		for(var i = 0; i < l; i++)
		{
			c = childTables[i]; 
			if(c.getID() == id)
				return c; 
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
	this.Unload = Unload; 		
	function Count() { return tables.length; }		
	function Add(gridEXTable) { tables[tables.length] = gridEXTable; }	
	function Unload()
	{
		for(var i=0;i<tables.length;i++)
		{
			var t = tables[i];
			t.Unload(); 
			t = null;
		}
		delete tables;
		tables = null;
		gridEX = null; 
	}
	function getIndexOf(id)
	{
		var l = tables.length;
		for(var i=0; i<l; i++)
		{
			if(tables[i].getID() == id)
				return i; 
		}
		return -1; 
	}
	function getTableByID(id)
	{		
		var l = tables.length; 
		var t = null; 
		for(var i=0; i<l; i++)
		{			
			t = tables[i];
			if(t.getID() == id) 
				return t; 
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
	this.Unload = Unload;
	this.updateColumnsDefinition = updateColumnsDefinition; 
	function Add(columnSet) {	 columnSets[columnSets.length] = columnSet; }	
	function AutoSizeColumns(headers)
	{
		if(isinHeader)
			AutoSizeByHeaders(headers);		
	}	
	function AutoSizeColumnsByHeader(_header)
	{	
		if(columnSets.length > 0)
		{
			var l = columnSets.length; 			
			var innerColumnSet = null; 			
			var _innerColumnSet = null; 
			var cell = null; 
			var _cell = null; 
			var k = -1; 
			for(var i = 0; i < l; i++)
			{
				_innerColumnSet   = _header.getColumnSets().getColumnSetInIndex(i).getHtmlColumnSet(); 
				innerColumnSet = getColumnSetInIndex(i).getHtmlColumnSet(); 
				k = _innerColumnSet.cells.length; 
				for(var j = 0; j < k; j++)
				{
					_cell = _innerColumnSet[j]; 
					if(_cell.type != "space")
					{
						cell = innerColumnSet[j]; 
						cell.style.pixelWidth = _cell.offsetWidth; 
						cell.childNodes[0].style.pixelWidth = _cell.childNodes[0].offsetWidth;
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
	function getCount() { return columnSets.length; }	
	function getGridEX() { return getGridEXTable().getGridEX(); }	
	function getGridEXHeader() { return gridEXHeader; }	
	function getGridEXTable() { return gridEXTable; }	
	function getColumnSetsWidth()
	{		
		var columnsetswidth = 0; 
		var r = parentElement.rows[0]; 
		var l = r.cells.length; 
		for(var i = 0; i < l; i++)
		{		
			if(r.cells[i].type != "rh")
				columnsetswidth += r.cells[i].offsetWidth;
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
			if(r.cells[i].type != "rh" && r.cells[i].getAttribute("allowsize") != null)
				w += r.cells[i].offsetWidth;
		}
		return w;
	}
	function getColumnSetsCoreWidth()
	{
		var columnsetswidth = 0; 
		var l = columnSets.length; 
		for(var i = 0; i < l; i++)
			columnsetswidth += columnSets[i].getHtmlColumnSet().offsetWidth; 
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
		for(var i = 0; i < _cellslength; i++)
		{	
			cell = row.cells[i]; 
			if(cell.type != "rh" && cell.getAttribute("allowsize") == null)
				originalsets[originalsets.length] = cell.offsetWidth;								
		}		
		var igset = 0; 
		var igcell = 0;
		var borderwidth = null;
		var oldsize = null; 
		var newsize = null; 		
		var _cellIndex = null; 		
		for(var i = 0; i < _cellslength; i++)
		{
			cell = row.cells[i]; 
			if(cell.type != "rh" && cell.getAttribute("allowsize") == null)
			{				
				borderwidth = getBorderWidth(cell); 
				oldsize = originalsets[igset];
				newsize = Math.round((oldsize * newwidth) / oldwidth);					
				oldsize -= borderwidth;
				newsize -= borderwidth;				
				_cellIndex = parseInt(cell.index, 10); 				
				columnset = getColumnSetInIndex(_cellIndex);
				columnset.AutoSize(oldsize, newsize, igcell, originalcells, headers);				
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
		var cellsLength = tableCells.cells.length; 
		var _cell = tableCells.cells[cell]; 
		var _low = parseInt(_cell.getAttribute("usecol"), 10); 
		var _high = _low + _cell.colSpan; 	
		for(var icell=0;icell<cellsLength;icell++)
		{
			if(icell != cell)
			{
				_cell = tableCells.cells[icell]; 
				if(_cell.type != "space" && _cell.type != "header")
				{	
					usecol = parseInt(_cell.usecol, 10); 
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
	function FixAutoSizeWidth(width, headers)
	{
		var row = parentElement.rows[0]; 		
		var columnsetswidth = getColumnSetsCoreWidth() - getFixedColumnSetsWidth();
		var diffsize = width - columnsetswidth;  		
		var offset; 
		if(diffsize < 0) 
			offset = -1;
		else if(diffsize > 0) 
			offset = 1;
		else 
			return; 		
		
		var _diff = 0; 
		var _cellsLength = row.cells.length; 
		var _innercellsLength = null; 
		var cell = null; 
		var columnset = null; 
		var innercell = null; 
		var innercolumnset = null;
		var _columnsetIndex = null; 
		var _lowwidth = 0; 
		var _header = null; 		
		var _headersLength = (headers != null) ? headers.length : -1; 
		var _cols = null;
		var olddiff = -1; 
		var colspan = -1; 
		var _fixedwidth = -1;
		var trycount = 0; 
		do
		{
			olddiff = diffsize;
			for(var i = 0; i < _cellsLength && diffsize != 0 && trycount < 3; i++)
			{
				cell = row.cells[i];
				if(cell.type != "rh" && cell.getAttribute("allowsize") == null)
				{
					_columnsetIndex = parseInt(cell.index, 10); 
					columnset = getColumnSetInIndex(_columnsetIndex);
					innercolumnset = columnset.getHtmlColumnSet(); 
					_innercellsLength = innercolumnset.cells.length; 
					_cols = innercolumnset.getElementsByTagName("COL");
					var fixedcols = new Array(_cols.length); 
					for(var icell = 0; icell < _innercellsLength; icell++)
					{
						innercell = innercolumnset.cells[icell];						
						if(innercell.type != "space" && innercell.allowsize == null)
						{
							if(innercell.colSpan != columnset.getColumnCount() || cellCouldResizeOthers(icell, innercolumnset, _innercellsLength))
							{
								if(diffsize != 0)
								{							
									if(innercell.type != null && innercell.type == "ch")
										_lowwidth = getGridEXTable().getHeaderWidth();
									else
									{
										if(innercell.getAttribute("pec") != null)
											_lowwidth = 18;
										else
											_lowwidth = 0; 
									}
									var usecol = parseInt(innercell.usecol, 10); 
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
													columnsetswidth += offset; 
													diffsize = width  - columnsetswidth;
												}
												fixedcols[icol] = 1; 
											}
										}
										_fixedwidth += getPixelColWidth(_cols[icol].width);
									}
									var diff = getPaddingLeft(innercell) + getPaddingRight(innercell) + getBorderWidth(innercell) + getSortWidth(innercell);
									if(_fixedwidth - diff > _lowwidth)
										innercell.childNodes[0].style.pixelWidth = _fixedwidth - diff; 							
								}
								else
								{
									colspan = innercell.colSpan + parseInt(innercell.usecol, 10); 
									_fixedwidth = 0; 
									for(var icol = parseInt(innercell.usecol, 10); icol < colspan; icol++)
										_fixedwidth += getPixelColWidth(_cols[icol].width); 
									var diff = getPaddingLeft(innercell) + getPaddingRight(innercell) + getBorderWidth(innercell) + getSortWidth(innercell);
									if(_fixedwidth - diff > _lowwidth)
										innercell.childNodes[0].style.pixelWidth = _fixedwidth - diff;
								}
							}							
						}
						else if(innercell.allowsize != null)
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
	}	
	function AutoSizeItems()
	{
		var l = columnSets.length; 
		for(var i = 0; i < l; i++)
			getColumnSetInIndex(i).AutoSizeItems();
		if(getGridEX().getFixTableSize())
			fixTableSize(getGridEX().getRootTable()); 
	}	
	function updateColumnsDefinition()
	{		
		var _field = document.getElementById(getGridEXTable().getID() + "_cols");
		if(_field == null)
			throw Error("input field for columns definition is null");			
		
		var cell = null;		
		var columnSetsLength = columnSets.length; 
		var columnset = null;
		var innercolumnset = null;		
		var _cols = null; 
		var _length = null;		
		var _width = -1;
		for(var icolumnset = 0; icolumnset < columnSetsLength; icolumnset++)	
		{			
			columnset = columnSets[icolumnset]; 
			innercolumnset = columnset.getHtmlColumnSet();	
			_length = innercolumnset.cells.length; 			
			_cols = innercolumnset.getElementsByTagName("COL"); 
			for(var index = 0; index < _length; index++)
			{			
				cell = innercolumnset.cells[index]; 
				if(cell.allowsize == null)
				{
					if((cell.type != "header" && cell.type != "space") && (cell.id != null && cell.usecol != null))
					{
						var _icol = parseInt(cell.usecol, 10); 
						_width = getPixelColWidth(_cols[_icol].width); 						
						if(cell.type == "ch")
							_width -= getGridEXTable().getHeaderWidth();
						updateColumnDefinitionInField(_field, cell.id, -1, _width, cell.childNodes[0].offsetWidth);
					}
				}						
			}
		}
	}	
	function Unload()
	{
		unloadObjectsArray(columnSets);
		columnSets = null;
		delete columnSets;
	}
	var row = parentElement.rows[0];
	var l = row.cells.length; 
	for(var i=0; i<l; i++)
	{
		var c = row.cells[i];
		if(c.type == "rh" || c.getAttribute("allowsize") != null)
			fixedwidth += c.offsetWidth; 
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
	this.Unload = Unload;
	this.column_onmousemove = column_onmousemove;
	this.column_onmouseover  = column_onmouseover;
	this.column_onmousedown = column_onmousedown;
	this.column_onmouseup = column_onmouseup;
	this.column_onclick = column_onclick;
	this.column_ondblclick = column_ondblclick;
	function getColumnCount()
	{		
		if(htmlTable.cc != null)
			return parseInt(htmlTable.cc, 10);
		else
			return 0; 
	}	
	function getGridEXHeader() { return gridEXHeader; }	
	function getGridEX() { return getGridEXTable().getGridEX();  }	
	function getGridEXTable() { return gridEXTable; }	
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
		_cellsLength = htmlTable.cells.length;
		_cols = htmlTable.getElementsByTagName("COL");		
		var colspan = 0; 
		var _fixedwidth = 0; 
		for(var i = 0; i < _cellsLength; i++)
		{			
			cell = htmlTable.cells[i];
			if(cell.allowsize != null && cell.usecol != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
				_fixedwidth[cell.usecol] = cell.offsetWidth; 
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
		for(var i = 0; i < _cols.length; i++)
			_oldcols[i] = getPixelColWidth(_cols[i].width);	
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			cell = htmlTable.cells[icell];
			if(cell.type != "space" && cell.allowsize == null)
			{
				if(cell.type == "header" || cell.colSpan == getColumnCount())
				{								
					diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);
					if(newwidth - diff > 0)
					{
						if(cell.type == "header")	
							cell.style.pixelWidth = newwidth - diff;
						else
						{	
							_fixedwidth = 0; 
							colspan = cell.colSpan + parseInt(cell.usecol, 10); 							
							for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
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
								cell.childNodes[0].style.pixelWidth = _fixedwidth - diff;													
						}
						if(headers != null)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 				
					}
				}
				else
				{	
					_fixedwidth = 0; 
					colspan = cell.colSpan + parseInt(cell.usecol, 10);
					for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
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
					if(_fixedwidth - diff > 0)
						cell.childNodes[0].style.pixelWidth = _fixedwidth - diff; 															
					if(headers != null)
						ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 
				}
			}					
		}
	}	
	function ResizeColumnSetHeader(column, posX)
	{	
		var _rtl = getGridEXTable().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1";		
		var offsetwidth = -1;
		posX += getScrollLeft(getGridEXTable().getGridEX()); 
		if(_rtl)
		{			
			posX -= fixRightToLeftScroll();
			posX -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft); 
			offsetwidth = getPixelLeft(column) - posX; 
		}
		else
		{
			if(column.getAttribute("mrc") != null)
				offsetwidth = posX - (getPixelLeft(column) + column.offsetWidth); 
			else
				offsetwidth = posX -  (getPixelLeft(htmlTable) + htmlTable.offsetWidth);
		}
		if(!getGridEXHeader().getIsRoot())
			offsetwidth += getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
		if(!_rtl && offsetwidth  > 0)
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
					
				getGridEXHeader().getColumnSets().updateColumnsDefinition();
			}
		}
		else
		{		
			if((column.type != null && column.type == "header") || column.colSpan == getColumnCount() || isMostRight(column, _rtl))
			{
				var oldsize = htmlTable.offsetWidth;
				var newsize = oldsize + offsetwidth;
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
				var cellsize = -1;
				var oldsize = column.offsetWidth;
				if(_rtl)
					cellsize = oldsize + offsetwidth; 
				else
					cellsize = oldsize - (column.offsetWidth + getPixelLeft(column) - posX); 
				offsetwidth = cellsize - oldsize;					
				var oldwidth = htmlTable.offsetWidth;
				var newwidth = -1; 
				if(getGridEX().getColumnAutoResize())
					newwidth = oldwidth; 
				else
					newwidth = htmlTable.offsetWidth + offsetwidth;					
				var oldremain = oldwidth - oldsize;
				var newremain = oldwidth - cellsize;		
				if(cellsize > getMinimalWidth(column))
				{		
					ResizeCellsInSet(column, cellsize, oldsize, oldremain, newremain);				
					AutoSizeItems(); 
					getGridEXHeader().getColumnSets().updateColumnsDefinition(); 
				}
			}
		}		
		resetRootTableScroll(getGridEX().getRootTable()); 			
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
				_cell = _htmlColumnSet.cells[columnIndex];
				if((column.type == "ch" || column.isCH != null) && (_cell.type != "ch" && _cell.isCH == null))
				{						
					var _width = getPixelColWidth(cols[parseInt(column.usecol, 10)].width) - getGridEXTable().getHeaderWidth();
					if(_cell.getAttribute("pec") != null && _cell.type != "ch")
						_width += 18;											
					_cols[parseInt(_cell.usecol, 10)].width = _width + "px";
					if(_cell.colSpan > 1)
					{
						var colspan = _cell.colSpan + parseInt(_cell.usecol, 10); 
						for(var icol = parseInt(_cell.usecol, 10) + 1; icol < colspan; icol++)
						{
							_cols[icol].width = cols[icol].width; 
							_width += getPixelColWidth(_cols[icol].width); 
						}
					}
					var diff = getPaddingLeft(_cell) + getPaddingRight(_cell) + getBorderWidth(_cell) + getSortWidth(_cell);
					_cell.childNodes[0].style.pixelWidth = (_width - diff);					
				}
				else if((column.type != "ch" && column.isCH == null) && (_cell.type == "ch" || _cell.isCH != null))
				{
					var _width = getPixelColWidth(cols[parseInt(column.usecol, 10)].width) + getGridEXTable().getHeaderWidth(); 
					if(_cell.getAttribute("pec") != null && _cell.type != "ch")
						_width += 18;
					_cols[parseInt(_cell.usecol, 10)].width = _width + "px"; 	
					if(_cell.colSpan > 1)
					{						
						var colspan = _cell.colSpan + parseInt(_cell.usecol, 10); 
						for(var icol = parseInt(_cell.usecol, 10) + 1; icol < colspan; icol++)
						{
							_cols[icol].width = cols[icol].width; 
							_width += getPixelColWidth(_cols[icol].width); 
						}
					}
					var diff = getPaddingLeft(_cell) + getPaddingRight(_cell) + getBorderWidth(_cell) + getSortWidth(_cell);
					_cell.childNodes[0].style.pixelWidth = (_width - diff);					
				}
				else
				{
					var colspan = _cell.colSpan + parseInt(_cell.usecol, 10); 
					var _width = 0; 
					for(var icol = parseInt(_cell.usecol, 10); icol < colspan; icol++)
					{
						_cols[icol].width = cols[icol].width; 
						_width += getPixelColWidth(_cols[icol].width); 
					}
					var diff = getPaddingLeft(_cell) + getPaddingRight(_cell) + getBorderWidth(_cell) + getSortWidth(_cell);
					_cell.childNodes[0].style.pixelWidth = (_width - diff); 
				}
			}
		}
	}
	function Unload()
	{	
		gridEXTable = null;
		gridEXHeader = null;
		var l = htmlTable.rows.length; 
		var _row = null; 
		for(var j = 0; j < l; j++)
		{
			_row = htmlTable.rows[j]; 
			g = _row.cells.length; 
			for(var i = 0; i < g; i++)
			{
				cell = _row.cells[i];
				if(cell.type != "space")
				{									
					cell.detachEvent("onmousemove", hcscolumn_onmousemove); 
					cell.detachEvent("onmouseover", hcscolumn_onmouseover);					
					cell.detachEvent("onmousedown", hcscolumn_onmousedown);  
					cell.detachEvent("onmouseup", hcscolumn_onmouseup); 
					cell.detachEvent("onselectstart", hcscolumn_onselectstart); 
					cell.detachEvent("onclick", hcscolumn_onclick);
					cell.detachEvent("ondblclick", hcscolumn_ondblclick); 
					cell.detachEvent("oncontextmenu", hcscolumn_oncontextmenu);
					cell.removeAttribute("columnset");			
				}
			}
		}
		cellsch = null;
		htmlTable = null; 
	}
	function cellCouldResizeOthers(cell, cellsLength)
	{		
		var _cell = htmlTable.cells[cell]; 
		var _low = parseInt(_cell.getAttribute("usecol"), 10); 
		var _high = _low + _cell.colSpan; 	
		for(var icell=0;icell<cellsLength;icell++)
		{
			if(icell != cell)
			{
				_cell = htmlTable.cells[icell]; 
				if(_cell.type != "space" && _cell.type != "header")
				{	
					usecol = parseInt(_cell.usecol, 10); 
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
		var _cellsLength = htmlTable.cells.length;						
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var colswidth = new Array(_cols.length); 
		var inallheaders = getGridEXTable().getHeaders().length > 1;
		var _fixedwidth = null; 
		for(var icell = 0; icell < _cellsLength; icell++)
		{			
			cell = htmlTable.cells[icell];
			if(cell.allowsize != null && cell.usecol != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
				_fixedwidth[cell.usecol] = cell.offsetWidth; 
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
			cell = htmlTable.cells[icell]; 
			if(cell.type != "space" && cell.allowsize == null)
			{
				if(cell.type == "header" || cell.colSpan == getColumnCount()) 
				{				
					diff = getPaddingLeft(cell) + getPaddingRight(cell)  + getBorderWidth(cell) + getSortWidth(cell); 					
					if(cell.type == "header")
						cell.style.pixelWidth = newsize - diff;					
					else if(cellCouldResizeOthers(icell, _cellsLength))
					{						
						var colspan = cell.colSpan + parseInt(cell.usecol, 10); 
						var _fixedwidth = 0; 
						for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
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
						if(_fixedwidth - diff > 0)
							cell.childNodes[0].style.pixelWidth = _fixedwidth - diff;
							
						if(inallheaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 
					}					
				}
				else 
				{		
					var colspan = cell.colSpan + parseInt(cell.usecol, 10); 
					var _fixedwidth = 0; 
					for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
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
					if(_fixedwidth - diff > 0)
						cell.childNodes[0].style.pixelWidth = _fixedwidth - diff;						
					if(inallheaders)
						ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols);
				}		
			}
		}
	}	
	function AutoSize(oldsize, newsize, igcell, cells, headers)	
	{	
		var _cellsLength = htmlTable.cells.length;
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var cell = null; 
		var cellwidth = null; 
		var diff = null; 		
		var _oldwidth = new Array();
		var _fixedwidth = null;
		for(var i = 0; i < _cellsLength; i++)
		{
			cell = htmlTable.cells[i]; 
			if(cell.type != "space" && cell.type != "header" && cell.allowsize == null)				
				_oldwidth[_oldwidth.length] = cell.offsetWidth;
			else if(cell.allowsize != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
					
				_fixedwidth[cell.usecol] = cell.offsetWidth; 
			}
		}
		var _oldcols = new Array(_cols.length); 
		for(var i = 0; i < _cols.length; i++)
			_oldcols[i] = getPixelColWidth(_cols[i].width);		
		
		if(_fixedwidth != null)
		{
			for(var i= 0; i < _fixedwidth.length; i++)
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
			cell = htmlTable.cells[icell]; 
			if(cell.type != "space" && cell.allowsize == null)
			{				
				if(cell.type == "header")
					cellwidth = newsize;
				else
				{
					cellwidth = Math.round((_oldwidth[_icell] * newsize) / oldsize); 
					_icell++;
				}					
				diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);
				if(cellwidth - diff > 0)
				{					
					if(cell.type == "header")
						cell.style.pixelWidth = cellwidth - diff;
					else
					{	
						if(cell.colSpan != getColumnCount() || cellCouldResizeOthers(icell, _cellsLength))
						{																	
							if(cell.usecol != null)
							{
								var colspan = cell.colSpan + parseInt(cell.usecol, 10); 
								var _fixedwidth = 0; 
								for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
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
								if(_fixedwidth - diff > 0)
									cell.childNodes[0].style.pixelWidth = _fixedwidth - diff;
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
		var _hw = 0; 
		for(var i = 0; i < _colsLength; i++)
		{
			var w = getPixelColWidth(_cols[i].width);
			_colswidth[_colswidth.length] = w;
			_hw += w;
		}
		htmlTable.parentElement.style.pixelWidth = _hw; 
		var _cellsLength = htmlTable.cells.length; 
		var _checkedCols = new Array(_cellsLength); 
		for(var _icell = 0; _icell < _cellsLength; _icell++)
		{
			_cell = htmlTable.cells[_icell]; 
			if(_cell.type == "ch" && _cell.usecol != null)
			{	
				var _usecol = parseInt(_cell.usecol, 10); 						
				if(_checkedCols[_usecol] == null)
				{
					_colswidth[_usecol] -= getGridEXTable().getHeaderWidth(); 				
					_checkedCols[_usecol] = -1; 
				}
			}
		}
		var _itemCol = null; 
		var _itemsCols = null;
		var _itemsTables = document.getElementsByName(getGridEXTable().getID() + "_items_cs" + getIndex()); 
		var _itemsTablesLength  = _itemsTables.length; 
		if(_itemsTables.length > 0)
		{			
			for(var _item = 0; _item < _itemsTablesLength; _item++)
			{
				_itemsCols = _itemsTables[_item].getElementsByTagName("COL"); 
				for(var i = 0; i < _colsLength; i++)
				{
					_itemCol = _itemsCols[i]; 
					if(_itemCol.getAttribute("iscz") != null)
					{
						var _width = _colswidth[i] + getGridEXTable().getHeaderWidth(); 
						_itemCol.width = _width + "px"; 
					}
					else
						_itemCol.width = (_colswidth[i]) + "px";
				}
				_itemsTables[_item].parentElement.style.pixelWidth = _hw; 
			}			
		}
		if(getGridEXTable().getParent() == null)
		{			
			var _htmlitemstable = getGridEXTable().getHtmlItemsTable(); 
			if(_htmlitemstable.getAttribute("empty") != null)
				_htmlitemstable.style.pixelWidth = getGridEXTable().getWidth(); 
		}
		var _newdiv = document.getElementsByName("nrsep" + getGridEXTable().getID()); 
		if(_newdiv != null)
		{
			for(var i = 0; i < _newdiv.length; i++)
				_newdiv[i].style.pixelWidth = getGridEXTable().getWidth(); 
		}
		var thdiv = document.getElementsByName("th" + getGridEXTable().getID());
		if(thdiv != null)
		{
			for(var i=0;i<thdiv.length;i++)	
			{
				var thwidth = getGridEXTable().getWidth();
				if(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset") != null)
					thwidth -= parseInt(thdiv[i].getElementsByTagName("COL")[0].getAttribute("offset"), 10); 
				thdiv[i].getElementsByTagName("COL")[0].width = thwidth + "px";
			}
		}
		if(getGridEXTable().getGridEX().getResizeGroups())
		{
			_itemsTables = document.getElementsByName("group" + getGridEXTable().getID()); 			
			var _offset = 0; 
			var _tableWidth = getGridEXTable().getWidth(); 
			for(var i = 0; i < _itemsTables.length; i++)
			{
				_itemsCols = _itemsTables[i].getElementsByTagName("COL"); 
				_itemCol = _itemsCols[0];
				_offset = _itemCol.getAttribute("offset"); 
				_itemCol.width = (_tableWidth - _offset) + "px";				
			}			
		}
		if(getGridEXTable().getUseColumnSets() && getGridEXTable().getPreviewRow())
		{
			var _previewTables = document.getElementsByName("preview" + getGridEXTable().getID()); 
			var _tableWidth = getGridEXTable().getWidth(); 
			for(var i = 0; i < _previewTables.length; i++)
			{
				var _previewCol = _previewTables[i].getElementsByTagName("COL")[0];
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
		var l = htmlTable.cells.length;
		var cell = null; 
		for(var i = 0; i < l; i++)
			cells[cells.length] = htmlTable.cells[i].offsetWidth;
	}
	function column_onmousedown()
	{
		if(window.event.button == 1)			
		{	
			var column = getColumnFromElement(window.event.srcElement);							
			if(column.type == "space")
				return;				
			if(window.event.srcElement != null && window.event.srcElement.tagName == "INPUT")
				return; 				
			if((column.id == null || column.id.length == 0) && (column.type != "header"))
				return;
			if(!columnResizing)
			{
				if(column.type == "header")
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
						else	if(gridEXColumn.getAllowDrag()) 
						{					
							if(column.type != null && column.type == "header")
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
		}
	}	
	function column_onmousemove()
	{		
		if(couldStartDrag && dragpoint != null)
		{
			if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
			{
				var column = null;
				if(couldDragColumn != null)
					column = getColumnFromElement(couldDragColumn); 
				else 
					column = getColumnFromElement(window.event.srcElement); 
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
			if(column.allowsize == null) 
			{
				if(isInResizeArea(column))
					column.style.cursor = cursorResize;
				else
					column.style.cursor = "default"; 
			}
		}
	}		
	function column_onmouseover()	
	{
		var column = getColumnFromElement(window.event.srcElement);
		if(column.allowsize == null) 
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
	function column_onmouseup() { eventButton = window.event.button; }
	function column_onclick()
	{		
		if(eventButton == 1)
		{
			var tdColumn = getColumnFromElement(window.event.srcElement); 
			if(!columnResizing && !columnDraging && !canceledByUser && tdColumn.style.cursor != cursorResize)
			{					
				if(tdColumn.type != null && (tdColumn.type == "header" || tdColumn.type == "space"))
					return; 
				
				var column = getGridEXTable().getColumns().getGridEXColumnByClientID(tdColumn.id); 
				if(column.getAllowSort())
				{			
					if(column.getActAsSelector())
						return; 	
						
					var cancel = getGridEX().FireEvent("ColumnHeaderClick", [column]);
					if(cancel == null || !cancel)
					{
						var input = document.getElementById(getGridEX().getID() + "_eventdata"); 
						if(input == null)
							throw Error("sort info field is null");
						
						input.value = getGridEXTable().getID() + ":" + column.getClientID();
						getGridEX().DoPostBack(null, "ColumnHeaderClick");
					}
				}
			}
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
		}
		getGridEXTable().getGridEX().setHitTestArea(6); 
		getGridEXTable().getGridEX().FireEvent("Click", [getGridEXTable().getGridEX(),eventButton, window.event.clientX, window.event.clientY]); 				
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
	function column_ondblclick()
	{		
		cancelColumnSetResize();
		var column = getColumnFromElement(window.event.srcElement); 
		if(column.type == "header") 
			return; 
			
		if(column.style.cursor == cursorResize)
		{
			var gridEXColumn = getGridEXTable().getColumns().getGridEXColumnByClientID(column.id);
			AutoSizeColumn(gridEXColumn, column);
		}
		getGridEXTable().getGridEX().setHitTestArea(6); 
		getGridEXTable().getGridEX().FireEvent("DoubleClick", [getGridEXTable().getGridEX(), window.event.clientX, window.event.clientY]); 
	}		
	function getFixedWidth()
	{		
		var fixedwidth = 0;
		for(var i = 0; i < htmlTable.cells.length; i++)
		{
			var cell = htmlTable.cells[i];
			if(cell.allowsize != null) 
				fixedwidth += cell.offsetWidth; 
		}
		return fixedwidth;
	}	
	function isMostRight(td, rtl)
	{		
		if(td.getAttribute("mrc") != null)
			return true; 
		else if((rtl == null || !rtl) && (td.offsetLeft + td.offsetWidth >= htmlTable.offsetWidth) )
			return true;
		else if(rtl != null && rtl && td.offsetLeft == 0)
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
			if(getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			{
				xlow = -1;
				xhigh = 5;
			}
			else
			{
				xlow = td.offsetWidth - 4;
				xhigh = td.offsetWidth + 4; 		
			}
		}
		else
		{
			if(getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			{
				xlow = -1;
				xhigh = 5;
			}	
			else
			{
				xlow = td.offsetWidth - 5;
				xhigh = td.offsetWidth; 
			}
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
		var _cellsLength = htmlTable.cells.length;
		var _cols = htmlTable.getElementsByTagName("COL"); 
		var _inallHeaders = getGridEXTable().getHeaders().length > 1; 				
		var _fixedwidth = null;
		var col = parseInt(column.usecol, 10); 
		for(var i = 0; i < _cellsLength; i++)
		{
			cell = htmlTable.cells[i];
			if(cell.allowsize != null && cell.usecol != null)
			{
				if(_fixedwidth == null)
					_fixedwidth = new Array(); 
				_fixedwidth[cell.usecol] = cell.offsetWidth; 
			}
		}
		var colswidth = new Array(_cols.length); 		
		for(var i = 0; i < _cols.length; i++)
			colswidth[i] = getPixelColWidth(_cols[i].width); 
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
		var colspan = column.colSpan + parseInt(column.usecol, 10); 		
		for(var icol = parseInt(column.usecol, 10); icol < colspan; icol++)
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
		if(fixedwidth - diff > 0)
			column.childNodes[0].style.pixelWidth = fixedwidth - diff; 		
		else
			column.childNodes[0].style.pixelWidth = 0;			
		for(var icell = 0; icell < _cellsLength; icell++)
		{
			cell = htmlTable.cells[icell];
			if(cell != column)
			{
				if(cell.allowsize == null)
				{
					if(cell.type == "header" || cell.colSpan == getColumnCount())
					{									
						if(cell.type == "header")
							cell.style.pixelWidth = cellsize; 
						else if(cellCouldResizeOthers(icell, _cellsLength))
						{						
							fixedwidth = 0; 
							colspan = cell.colSpan + parseInt(cell.usecol, 10); 
							for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
							{
								if(fixedcols[icol] == null)
								{
									_cols[icol].width = Math.round((colswidth[icol] * newsize) / oldsize) + "px"; 							
									fixedcols[icol] = 1;
								}
								fixedwidth += getPixelColWidth(_cols[icol].width); 
							}
							diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell);
							if(fixedwidth - diff > 0)
								cell.childNodes[0].style.pixelWidth = fixedwidth - diff;
							else
								cell.childNodes[0].style.pixelWidth = 0; 
						}
						if(_inallHeaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 
					}			
					else if(cell.usecol != null && cell.usecol == col && cell.colSpan == column.colSpan)
					{									
						fixedwidth = 0; 
						colspan = cell.colSpan + parseInt(cell.usecol, 10); 
						for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
						{
							if(fixedcols[icol] == null)
							{							
								_cols[icol].width = Math.round((colswidth[icol] * cellsize) / oldcellsize) + "px"; 							
								fixedcols[icol] = 1;
							}
							fixedwidth += getPixelColWidth(_cols[icol].width);
						}
						diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
						if(fixedwidth - diff > 0)
							cell.childNodes[0].style.pixelWidth = fixedwidth - diff;
						else
							cell.childNodes[0].style.pixelWidth = 1; 
						if(_inallHeaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols); 
					}
					else	 if(cell.type != "space")
					{	
						fixedwidth = 0; 
						colspan = cell.colSpan + parseInt(cell.usecol, 10); 
						for(var icol = parseInt(cell.usecol, 10); icol < colspan; icol++)
						{
							if(fixedcols[icol] == null)
							{
								_cols[icol].width = Math.round((colswidth[icol] * newsize) / oldsize) + "px";
								fixedcols[icol] = 1;
							}
							fixedwidth += getPixelColWidth(_cols[icol].width);
						}										
						diff = getPaddingLeft(cell) + getPaddingRight(cell) + getBorderWidth(cell) + getSortWidth(cell); 
						if(fixedwidth - diff > 0)
							cell.childNodes[0].style.pixelWidth = fixedwidth - diff;
						else
							cell.childNodes[0].style.pixelWidth = 0; 
						if(_inallHeaders)
							ResizeColumnInColumnSetHeaders(cell, icell, getIndex(), _cols);
					}			
				}
			}			
		}
	}	
	var fixedwidth = getFixedWidth();	
	var cellsch = null; 
	if(getIsInHeader())
	{
		var c = null;				
		htmlTable.parentElement.attachEvent("onselectstart", gcolumnset_onselectstart); 
		htmlTable.attachEvent("onselectstart", gcolumnset_onselectstart);		
		var g = -1; 
		var l = htmlTable.rows.length; 
		var _row = null; 
		for(var j=0; j<l; j++)
		{
			_row = htmlTable.rows[j]; 
			g = _row.cells.length; 
			for(var i=0; i<g; i++)
			{
				c = _row.cells[i];
				if(c.type != "space")
				{									
					c.attachEvent("onmousemove", hcscolumn_onmousemove); 
					c.attachEvent("onmouseover", hcscolumn_onmouseover);					
					c.attachEvent("onmousedown", hcscolumn_onmousedown);  
					c.attachEvent("onmouseup", hcscolumn_onmouseup); 
					c.attachEvent("onselectstart", hcscolumn_onselectstart); 
					c.attachEvent("onclick", hcscolumn_onclick); 
					c.attachEvent("ondblclick", hcscolumn_ondblclick); 
					c.attachEvent("oncontextmenu", hcscolumn_oncontextmenu);
					c.setAttribute("columnset", this);			
				}
				if(c.type == "ch")
				{
					if(cellsch == null)
						cellsch = new Array(); 
						
					cellsch[cellsch.length] = c; 					
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
	var columnSetColumn = -1;
	var editTarget = 2;
	var editType = -1; 
	var filterConditions = null; 
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
	this.Unload = Unload; 	
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
	function getKeepColumnExpand() {	return keepColumnExpand; }	
	function getKey() { return key; }	
	function getMaxLength() {	return maxlength; }	
	function getMultiLineEdit() { return multiLineEdit; }
	function getPasswordChar() { return passwordChar; }	
	function getPosition() { return this.position; }
	function getScrollBars() { return scrollbars; }	
	function getSelectable() { return selectable; }
	function getTable() { return table; }
	function getVisible() { return visible; }		
	function Unload() { delete table; }
	function AutoSize()
	{
		if(!getVisible() || !getAllowSize())
			return;
			
		table.AutoSizeColumn(gcolumn); 
	}
	var gcolumn = this; 
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
		unloadArray(columnArray); 
		delete columnArray;
		columnArray = null;
	}
	unloadArray(array); 
	delete array;
	array = null;
	var hiddenColumnsCount = null;
	this.getGridEXColumnByClientID = getGridEXColumnByClientID; 
	this.getGridEXColumn = getGridEXColumn;
	this.Count = Count;
	this.HiddenColumnsCount = HiddenColumnsCount;	
	this.Unload = Unload; 
	function Count() { return columns.length; }	
	function HiddenColumnsCount()
	{		
		if(hiddenColumnsCount == null)
		{			
			hiddenColumnsCount = 0; 			
			for(var i = 0; i < Count(); i++)
			{				
				if(!columns[i].getVisible())
					hiddenColumnsCount++;
			}		
		}		
		return hiddenColumnsCount; 
	}	
	function Unload()
	{
		unloadObjectsArray(columns); 
		delete columns;
		columns = null;
		table = null;
	}
	function getGridEXColumn(i)
	{
		if(i < 0 || i >= Count())
			throw Error("index out of range"); 
			
		return columns[i];
	}	
	function getGridEXColumnByClientID(id)
	{		
		var column = null; 
		var l = columns.length; 
		for(var i = 0; i < l; i++)
		{
			column = columns[i]; 
			if(column.getClientID() == id)
				return column; 
		}		
		throw Error("argument out of range"); 
	}
	return this; 
}
function GridEXCell(column, row)
{	
	var column = column; 
	var conditionOperator = 1; 
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
		innerCell = row.getInnerRow().all(column.getClientID() + "_L"); 
		if(innerCell == null)
			throw Error("unable to find cell object");		
			
		valueType = 1;
		if(column.getEditType() == 9 || column.getFilterEditType() == 19)
		{
			if(innerCell.childNodes[0].childNodes.length == 1)
			{
				var element = innerCell.childNodes[0].childNodes[0]; 
				if(element.nodeType == 1 && element.tagName == "INPUT" && element.type == "checkbox")
				{
					cellValue = element.checked;
					if(window.event.type == "click" && window.event.srcElement == element)
						cellValue = !cellValue; 
				}
			}
		}	
	}
	else if(row.getGridEX().getUseHiddenColumns())
	{
		if(row.getRowType() == "Record")
		{
			var hiddenValues = row.getGridEX().FindHiddenValuesByRow(row);		
			if(hiddenValues != null)
			{			
				for(var hiddenValue = 0; hiddenValue < hiddenValues.length; hiddenValue = hiddenValue + 2)
				{
					if(hiddenValues[hiddenValue] == column.getColumnIndex())
					{
						cellValue = normalizeValue(hiddenValues[hiddenValue+1]); 
						hiddenValue = hiddenValues.length; 
					}
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
	this.getValue = getValue;
	this.isNewRecordTopCell = isNewRecordTopCell; 
	this.setChecked = setChecked;
	this.setValue = setValue;	
	this.setText = setText;
	this.HideFilterCell = HideFilterCell; 
	this.ResumeEdit = ResumeEdit; 
	this.ResumeFilter = ResumeFilter; 
	this.ShowEditorCell = ShowEditorCell; 
	this.ShowFilterCell = ShowFilterCell; 
	this.UndoChanges = UndoChanges; 			
	this.Unload = Unload; 
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
			if(getColumn().getActAsSelector())
				return; 
				
			if(getColumn().getFilterEditType() == 19)
			{
				var _checkbox = innerCell.childNodes[0].childNodes[0];
				if(window.event.srcElement == _checkbox)
				{
					if(cellValue != _checkbox.checked)
					{
						this.dataChanged = true; 
						cellValue = _checkbox.checked; 
					}
				}
				_checkbox.focus();
				if(_checkbox.getAttribute("we") == null)
					attachCheckBoxEvent(_checkbox, getGridEX().getID()); 
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
					var _checkbox = innerCell.childNodes[0].childNodes[0]; 
					_checkbox.focus(); 
					if(_checkbox.getAttribute("we") == null)
						attachCheckBoxEvent(_checkbox, getGridEX().getID());
					
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
	function Unload()
	{
		innerCell = null;
		column = null;
		row = null;
		editManager = null;
		filterManager = null; 		
	}	
	function getColumn() { return column; }	
	function getCurrent() { return current; }	
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
				case 17:
					filterManager = new GridEXFilterComboDropDownManager(gridEXCell); 
				break;
				case 18:
					filterManager = new GridEXFilterDropDownManager(gridEXCell); 
				break; 
				case 19:
					filterManager = new GridEXFilterCheckBoxManager(gridEXCell); 
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
	function getGridEX() { return row.getGridEX(); }	
	function getInnerCell() { return innerCell; }	
	function getRow() { return row;  }
	function getTable() { return getRow().getTable(); }
	function getText()
	{
		if(innerCell == null || innerCell.childNodes.length == 0)
			return "";
		if(column.getEditType() == 9 || column.getFilterEditType() == 19)		
			return String(getValue());
		if(column.getPasswordChar().length > 0 && innerCell.getAttribute("text") != null)
			return innerCell.getAttribute("text");
		return trim(innerCell.childNodes[0].innerText); 
	}
	function setText(text)
	{
		if(innerCell == null || innerCell.childNodes.length == 0)
			return;
		if(column.getEditType() == 9 || column.getFilterEditType() == 19)
			return;
		if(column.getPasswordChar().length > 0 && innerCell.getAttribute("text") != null)
			innerCell.setAttribute("text", text);		
		if(innerCell.childNodes[0].childNodes.length == 1 && innerCell.childNodes[0].childNodes[0].tagName == "NOBR")	
		{
			if(column.getPasswordChar().length > 0)
				innerCell.childNodes[0].childNodes[0].innerText = PasswordText(text, column.getPasswordChar()); 
			else
				innerCell.childNodes[0].childNodes[0].innerText = text;
		}
		else
		{
			if(column.getPasswordChar().length > 0)
				innerCell.childNodes[0].innerText = PasswordText(text, column.getPasswordChar()); 
			else
				innerCell.childNodes[0].innerText = text;
		}
	}
	function getValue()
	{	
		if(valueType == 1) 
		{	
			if(getColumn().getColumnType() == 4 || getColumn().getActAsSelector()) 
			{
				if(innerCell.childNodes[0].childNodes.length > 0)
				{
					var element = innerCell.childNodes[0].getElementsByTagName("INPUT"); 
					if(element.length > 0)
					{
						for(var i = 0; i < element.length; i++)						
						{
							if(element[i].type == "checkbox")
								return element[i].checked; 								
						}
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
				if(innerCell.childNodes[0].childNodes.length > 0)
				{
					var element = innerCell.childNodes[0].getElementsByTagName("INPUT"); 
					if(element.length > 0)
					{
						for(var i = 0; i < element.length; i++)						
						{
							if(element[i].type == "checkbox")
							{
								element[i].checked = value; 
								ielement = element.length; 
							}
						}
					}
					else if(element.type == "checkbox")
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
	function isNewRecordTopCell() { return (getRow().getType() == 9 && getRow().getTable().getParent() == null && getRow().getTable().getNewRowPosition() == 2); }	
	function setChecked() { this.dataChanged = true; }	
	var gridEXCell = this; 	
	return this; 
}
function GridEXRow(id, innerRow, table, pos, rootRow)
{	
	var cells = null; 
	var childRows = -1; 
	var currentCell = null; 	
	var columnSetsCount = -1; 
	var currentColumnSet = null; 
	var currentColumnSetRow = null; 
	var dataKeyValues = null; 
	var headerIndicatorType	= -1; 
	var id = id;
	var ischecked = null;	
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
	
	if(innerRow.type != null)
		type = parseInt(innerRow.type, 10);
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
	this.getURL = getURL; 
	this.getURLTarget = getURLTarget;
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
	this.getVisibleInScroll = getVisibleInScroll;		
	this.setCurrentCell = setCurrentCell;	
	this.toogleIndicator = toogleIndicator;
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
	this.Unload = Unload; 
	function getGridEX() { return table.getGridEX(); }	
	function getID() { return id; }	
	function getTable() { return table; }	
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
		var c = null;
		var l = cells.length; 
		for(var i = 0; i < l; i++)
		{			
			c = cells[i]; 
			if(c.getColumn().getKey() == columnKey)
				return c; 
		}
		throw Error("argument out of range"); 
	}	
	function getCellByIndex(index)
	{
		RetrieveCells();
		if(index < 0 || index >= cells.length)
			throw Error("argument out of range");
			
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
					inspectedType = inspectedRow.type; 
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
	function getCurrentCell() { return currentCell; }
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
					var innerCell = getInnerRow().all(column.getClientID() + "_L"); 
					if(innerCell != null)
					{
						if(innerCell.childNodes[0].childNodes.length > 0)
						{
							var element = innerCell.childNodes[0].getElementsByTagName("INPUT"); 
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
	
		var url = getURL();
		if(url != null && url.length > 0)
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
	function getPreviewInnerRow()	 { return previewInnerRow; }	
	function getSelected()
	{
		if(getGridEX().getSelectedItems() != null)
			return getGridEX().getSelectedItems().IsRowSelected(gridEXRow); 

		return false; 
	}
	function getType() { return (type == 4) ? 3 : type; }	
	function getVisibleInScroll()
	{						
		if((getRootRowFromInner(getInnerRow()).offsetTop + getRowHeight()) > (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetHeight  + getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop))
			return false; 
		else if((getRootRowFromInner(getInnerRow()).offsetTop - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollTop) < 0)
			return false;
		else
			return true; 		
	}	
	function getVisibleRowHeader()
	{	
		var rtl = (getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		if(!rtl)
		{
			var minLeft = getPixelLeft(getGridEX().getRootTable().getHtmlItemsTable().offsetParent) + getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft; 
			if(headerIndicatorType == -1)
			{
				if((getPixelLeft(getRowHeaderCell()) + getRowHeaderCell().offsetWidth) >= minLeft)
					return true;
				else
					return false; 
			}
			else
			{	
				if(headerIndicatorType == 1)
				{
					if((getPixelLeft(getGridEX().getRowHeaderGlyph()) + getGridEX().getRowHeaderGlyph().offsetWidth) >= minLeft)
						return true;
					else
						return false;
				}
				else if(headerIndicatorType == 2)
				{				
					if((getPixelLeft(getGridEX().getRowHeaderEditGlyph()) + getGridEX().getRowHeaderEditGlyph().offsetWidth) >= minLeft)
						return true;
					else
						return false; 
				}
			}
		}
		else
		{
			var minLeft = getPixelLeft(getGridEX().getRootTable().getHtmlItemsTable().offsetParent) + getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth;			
			if(headerIndicatorType == -1)
			{
				var l = getPixelLeft(getRowHeaderCell()) + getGridEX().getRowHeaderGlyph().offsetWidth; 
				l += (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(l > minLeft)
					return false;
				else
					return true;				
			}
			else
			{
				if(headerIndicatorType == 1)
				{
					var l = getPixelLeft(getGridEX().getRowHeaderGlyph()) + getGridEX().getRowHeaderGlyph().offsetWidth;
					l += (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
					if(l > minLeft)
						return false;
					else
						return true; 
				}
				else if(headerIndicatorType == 2)
				{
					var l = getPixelLeft(getGridEX().getRowHeaderEditGlyph()) + getGridEX().getRowHeaderEditGlyph().offsetWidth;
					l += (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
					if(l > minLeft)
						return false;
					else
						return true;
				}
			}
		}
	}	
	function getDataChanged()
	{
		var t = getType(); 
		if(t == 3 || t == 4 || t == 9 || t == 11)
		{	
			RetrieveCells();
			var c = null;
			var l = cells.length; 
			for(var i = 0; i < l; i++)
			{			
				c = cells[i];
				if(c.getDataChanged() || (c.getInnerCell() != null && c.getInnerCell().getAttribute("ind") != null))
					return true; 
			}
		}
		return false; 
	}
	function ensureVisibleCell(cell)
	{
		if(cell == null)
			return; 		
		
		var sl = getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
		var cl = getPixelLeft(cell.getInnerCell());
		var cw = cell.getInnerCell().clientWidth; 
		var tl = getPixelLeft(getGridEX().getRootTable().getHtmlItemsTable().offsetParent);
		var tw = getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth;
		var isrtl = (getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		if(!isrtl)
		{
			if((getRowType() == "NewRecord" && getTable().getParent() == null && getTable().getNewRowPosition() == 2) || getRowType() == "FilterRow")
				cl += sl;				
			if((cl + cw - sl) > (tl + tw))
			{
				var scroll = Math.abs((cl + cw - sl) - (tl + tw));
				getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft += scroll;
			}
			else if(cl - sl < tl)
			{
				var scroll = cl - tl;
				getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft = scroll; 
			}
		}
		else
		{
			var ortl = (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			var xrtl = (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			var srtl = 0;
			srtl += ortl;
			srtl += xrtl;
			if((getRowType() == "NewRecord" && getTable().getParent() == null && getTable().getNewRowPosition() == 2) || getRowType() == "FilterRow")
				cl -= ortl; 
			if((cl+ortl) < (tl+xrtl))
			{
				var scroll = (tl + xrtl) - (cl + ortl);				
				getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft -= scroll; 
			}
			else if((cl+cw) > (tl+tw+xrtl-ortl))
			{
				var scroll = (cl+cw)-(tl+tw+xrtl-ortl);
				getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft += scroll; 
			}
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
			else if(currentCell.getInnerCell().currentStyle.textAlign == "right" || currentCell.getInnerCell().currentStyle.textAlign == "center")
				currentCell.getInnerCell().style.textAlign = currentCell.getInnerCell().currentStyle.textAlign; 
			if(getGridEX().focusCss != null)
			{
				if(currentCell.getRow().getTable().getUseColumnSets())
					currentCell.getInnerCell().setAttribute("className", currentCell.cssName + " " + getGridEX().focusCss);
				else
					currentCell.getInnerCell().setAttribute("className", getClassName(gridEXRow) + " " + getGridEX().focusCss);
			}				
		}
		getGridEX().currentCell = currentCell;
		if(getGridEX().currentCell != null)
			getGridEX().FireEvent("CurrentCellChanged", [getGridEX().currentCell]); 
		if(currentCell != null && currentCell.getColumn().getEditType() == -1 && currentCell.getInnerCell != null)
		{
			if(currentCell.getInnerCell().setActive != null)
				currentCell.getInnerCell().setActive();
		}
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
			if(cell.getColumn().getActAsSelector() && window.event.srcElement != null && window.event.srcElement.tagName == "INPUT" && window.event.srcElement.type == "checkbox") 
				CheckRow(cell.getValue(),cell.getColumn().getClientID(), true, window.event.srcElement.getAttribute("se") != null); 
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
			setCurrentCell(cell); 
			cell.ShowFilterCell();
		}
	}
	function CheckRow(checked,colID,reviewStatus,fireEvent)
	{
		if(getType() != 3)
			throw Error("invalid operation exception");
		
		if(cells == null)
		{
			var l =	getTable().getColumns().Count();
			for(var i=0;i<l;i++)
			{
				var column = getTable().getColumns().getGridEXColumn(i);
				if((colID == null && column.getActAsSelector() || column.getColumnType() == 4) || (column.getClientID() != colID && column.getActAsSelector() && column.getColumnType() == 4))
				{
					var innerCell = getInnerRow().all(column.getClientID() + "_L"); 
					if(innerCell != null)
					{						
						var element = innerCell.childNodes[0].getElementsByTagName("INPUT"); 
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
			for(var i=0; i<l; i++)
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
				if(headers.length > 0)
				{
					var l = headers.length; 
					for(var i = 0; i < l; i++)
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
					if(headers.length > 0)
					{
						var l = headers.length; 
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
						lastVisibleInspectedRow = getRootRowFromInner().rowIndex;
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
		{
			previewInnerRow.style.display = "none"; 
			previewInnerRow.cells[0].style.display = "none"; 
		}
		else
		{
			previewInnerRow.style.display = "block"; 		
			previewInnerRow.cells[0].style.display = "block"; 
		}		
		if(isExpanded)
			innerCell.childNodes[0].childNodes[0].src = getTable().getCollapsedPreviewRowGlyph(); 
		else
			innerCell.childNodes[0].childNodes[0].src = getTable().getExpandedPreviewRowGlyph();
			
		getGridEX().ReportRowsStatus();
	}	
	function getRowHeaderCell()
	{
		if(rowHeaderCell)
			return rowHeaderCell; 			
			
		var cell = null; 
		var cellHeader = null; 
		var i = 0; 
		var l = -1;		
		l = getInnerRow().cells.length; 
		while(cellHeader == null && i < l)
		{
			cell = getInnerRow().cells[i]; 
			if(cell.type == "rh")
				cellHeader = cell; 
			i++; 
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
			
		var _isrtl = getGridEX().getHtmlGridEX().getAttribute("rtl") == "1";
		headerTop = getPixelTop(cellHeader) - getVerticalScrollOffset(getGridEX()); 		
		headerLeft = getPixelLeft(cellHeader);
		if(!_isrtl)
			headerLeft -= getHorizontalScrollOffset(getGridEX());
		else
		{
			headerLeft += (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			if(!getGridEX().isHierarchicalGrid() && getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
				headerLeft += (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			else if(getGridEX().isHierarchicalGrid() && ((getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
				headerLeft += (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
		}
		headerWidth = cellHeader.offsetWidth;
		headerHeight = getInnerRow().offsetHeight; 							
		if(getPreviewInnerRow() != null)
			headerHeight += getPreviewInnerRow().offsetHeight; 

		return [headerLeft, headerTop, headerWidth, headerHeight]; 		
	}	
	function HideHeaderIndicator()
	{
		var headerIndicator = null; 
		if(getTable().getRowHeaders())
		{
			if(headerIndicatorType == 1)
			{
				headerIndicator = getGridEX().getRowHeaderGlyph(); 
				headerIndicator.style.visibility = "hidden"; 				
			}
			else if(headerIndicatorType == 2)
			{
				headerIndicator = getGridEX().getRowHeaderEditGlyph();
				headerIndicator.style.visibility = "hidden";
			}			
			if(getType() == 9)
			{
				var cellHeader = getRowHeaderCell();				
				cellHeader.childNodes[0].childNodes[0].style.visibility = "visible"; 
			}			
			headerIndicatorType = -1; 
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
	function ShowHeaderIndicator(editing)
	{
		var cellHeader = null; 
		var headerIndicator = null; 
		var headerSettings = null; 
		var _left = -1;
		var _top = -1; 
		if(getTable().getRowHeaders())				
		{							
			if(getType() == 5 || getType() == 8 || getType() == 11 || getType() == 12) 
				return; 
		
			if((getType() == 3 || getType() == 4 || getType() == 9) && ((editing != null && editing) || (getDataChanged() && getTable().getAllowEdit())))
			{
				if(headerIndicatorType == -1)
				{
					if(getVisibleInScroll() && getVisibleRowHeader())
					{						
						headerSettings = getHeaderSettings(); 
						if(headerSettings != null)
						{						
							if(getType() == 9)
							{
								cellHeader = getRowHeaderCell(); 
								cellHeader.childNodes[0].childNodes[0].style.display = "none"; 							
							}							
							headerIndicator = getGridEX().getRowHeaderEditGlyph();						
							_top = headerSettings[1] + (headerSettings[3] / 2) - (headerIndicator.offsetHeight / 2); 
							if(getType() == 9)
								_left = headerSettings[0] + (headerSettings[2] - headerIndicator.offsetWidth - 2); 
							else
								_left = headerSettings[0] + (headerSettings[2] / 2) - (headerIndicator.offsetWidth / 2);
							headerIndicator.style.pixelTop = _top; 
							headerIndicator.style.pixelLeft = _left;
							headerIndicator.style.visibility = "visible"; 							
							headerIndicatorType = 2; 
						}
					}					
					return; 
				}
				else if(headerIndicatorType == 1) 
				{
					headerIndicator = getGridEX().getRowHeaderGlyph(); 
					headerIndicator.style.visibility = "hidden"; 					
					if(getType() == 9)
					{							
						cellHeader = getRowHeaderCell(); 
						cellHeader.childNodes[0].childNodes[0].style.visibility = "hidden";
					}				
					if(getVisibleInScroll() && getVisibleRowHeader())
					{	
						headerIndicator = getGridEX().getRowHeaderEditGlyph(); 
						headerSettings = getHeaderSettings(); 
						if(headerSettings != null)
						{	
							_top = headerSettings[1] + (headerSettings[3] / 2) - (headerIndicator.offsetHeight / 2); 
							if(getType() == 9)
								_left = headerSettings[0] + (headerSettings[2] - headerIndicator.offsetWidth - 2);
							else
								_left = headerSettings[0] + (headerSettings[2] / 2)  - (headerIndicator.offsetWidth / 2);
							headerIndicator.style.pixelTop = _top; 
							headerIndicator.style.pixelLeft = _left; 
							headerIndicator.style.visibility = "visible";							
							headerIndicatorType = 2; 
						}
					}					
					return; 
				}
				else if(headerIndicatorType == 2)
				{
					if(!getVisibleInScroll() || !getVisibleRowHeader())
					{
						headerIndicator = getGridEX().getRowHeaderEditGlyph(); 
						headerIndicator.style.visibility = "hidden"; 
						return; 
					}					
					headerIndicator = getGridEX().getRowHeaderEditGlyph(); 
					headerSettings = getHeaderSettings(); 
					if(headerSettings != null)
					{	
						_top = headerSettings[1] + (headerSettings[3] / 2) - (headerIndicator.offsetHeight / 2); 
						if(getType() == 9)
							_left = headerSettings[0] + (headerSettings[2] - headerIndicator.offsetWidth - 2);
						else
							_left = headerSettings[0] + (headerSettings[2] / 2) - (headerIndicator.offsetWidth / 2); 
						headerIndicator.style.pixelTop = _top; 
						headerIndicator.style.pixelLeft = _left;
						headerIndicator.style.visibility = "visible";
					}					
					return; 
				}
			}
			else
			{
				if(headerIndicatorType == -1)
				{
					if(getVisibleInScroll() && getVisibleRowHeader())
					{						
						headerSettings = getHeaderSettings(); 
						if(headerSettings != null)
						{						
							if(getType() == 9)
							{
								cellHeader = getRowHeaderCell(); 
								cellHeader.childNodes[0].childNodes[0].style.visibility = "hidden";								
							}						
							headerIndicator = getGridEX().getRowHeaderGlyph(); 
							_top = headerSettings[1] + (headerSettings[3] / 2) - (headerIndicator.offsetHeight / 2);
							if(getType() == 9)
								_left = headerSettings[0] + (headerSettings[2] - headerIndicator.offsetWidth - 2);
							else
								_left = headerSettings[0] + (headerSettings[2] / 2) - (headerIndicator.offsetWidth / 2);
							headerIndicator.style.pixelTop = _top; 
							headerIndicator.style.pixelLeft = _left;
							headerIndicator.style.visibility = "visible";
						}
						headerIndicatorType = 1; 
					}					
					return; 
				}
				else if(headerIndicatorType == 1)
				{
					if(!getVisibleInScroll() || !getVisibleRowHeader())
					{
						headerIndicator = getGridEX().getRowHeaderGlyph(); 
						headerIndicator.style.visibility = "hidden"; 
						return; 
					}					
					headerSettings = getHeaderSettings(); 
					if(headerSettings != null)
					{					
						if(getType() == 9)
						{
							cellHeader = getRowHeaderCell(); 
							cellHeader.childNodes[0].childNodes[0].style.visibility = "hidden";
						}						
						headerIndicator = getGridEX().getRowHeaderGlyph(); 					
						_top = headerSettings[1] + (headerSettings[3] / 2) - (headerIndicator.offsetHeight / 2);
						if(getType() == 9)
							_left = headerSettings[0] + (headerSettings[2] - headerIndicator.offsetWidth - 2);
						else
							_left = headerSettings[0] + (headerSettings[2] / 2) - (headerIndicator.offsetWidth / 2); 												
						headerIndicator.style.pixelTop = _top; 
						headerIndicator.style.pixelLeft = _left; 
						headerIndicator.style.visibility = "visible";
					}					
					return; 
				}
				else if(headerIndicatorType == 2)
				{
					headerIndicator = getGridEX().getRowHeaderEditGlyph();
					headerIndicator.style.visibility = "hidden"; 					
					if(getVisibleInScroll() && getVisibleRowHeader())
					{						
						headerSettings = getHeaderSettings(); 
						if(headerSettings != null)
						{						
							if(getType() == 9)
							{
								cellHeader = getRowHeaderCell(); 
								cellHeader.childNodes[0].childNodes[0].style.visibility = "hidden";
							}							
							headerIndicator = getGridEX().getRowHeaderGlyph(); 
							_top = headerSettings[1] + (headerSettings[3] / 2) - (headerIndicator.offsetHeight / 2);
							if(getType() == 9)
								_left = headerSettings[0] + (headerSettings[2] - headerIndicator.offsetWidth - 2);
							else
								_left = headerSettings[0] + (headerSettings[2] / 2) - (headerIndicator.offsetWidth / 2); 							
							headerIndicator.style.pixelLeft = _left; 
							headerIndicator.style.pixelTop = _top; 
							headerIndicator.style.visibility = "visible";
						}
					}															
					headerIndicatorType = 1; 					
					return; 
				}
			}
		}
	}	
	function Unload()
	{		
		unloadObjectsArray(cells); 
		delete cells;
		cells = null;
		unloadArray(dataKeyValues); 
		innerRow = null;
		rootInnerRow = null;
		rootHeaderCell = null;
		previewInnerRow = null;
		table = null;
		gridEXRow = null;		
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
		var _cell = null;
		var _spans = null; 		
		for(var _tmpCell = 0; _tmpCell < _tmpCellsLength; _tmpCell++)
		{					
			_cell = _tmpCells[_tmpCell];						
			_spans = null; 
			if(getTable().getHierarchicalMode() == 1 || getType() == 8)
			{			
				if(_cell.name == "ec")
				{
					if(_cell.childNodes.length == 1 && _cell.childNodes[0].childNodes.length == 1 && _cell.childNodes[0].childNodes[0].tagName == "IMG")
						_spans = [_cell.childNodes[0]];
					else
						_spans = _cell.childNodes[0].all.tags("SPAN"); 
				}
			}
			else if(getTable().getHierarchicalMode() == 2 && getType() == 3)
			{
				if(_cell.type == "ec")
					_spans = _cell.childNodes[0].all.tags("SPAN"); 
			}			
			if(_spans != null && _spans.length > 0)
			{	
				_spansLength = _spans.length; 
				for(var _ispan = 0; _ispan < _spansLength; _ispan++)
				{
					_span = _spans[_ispan]; 
					if(_span.childNodes.length > 0 && _span.childNodes[0].tagName == "IMG")
					{						
						if(getType() == 3)
						{
							if(action == 0)
								_span.childNodes[0].src = eval("recordCollapse" + getGridEX().getID()); 
							else
								_span.childNodes[0].src = eval("recordExpand" + getGridEX().getID()); 					
								
							return true; 			
						}
						else if(getType() == 8)
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
							if(action == 0)
								_span.childNodes[0].src = eval("groupCollapse" + getGridEX().getID()); 
							else
								_span.childNodes[0].src = eval("groupExpand" + getGridEX().getID()); 														
								
							return true; 
						}
					}
					else
					{	
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
						if(_span.name == "lp" && action == 0)
						{
							_span.style.visibility = "hidden";
							return true;
						} 
						else if(_span.name == "lp" && action == 1)
						{
							_span.style.visibility = "visible"; 
							return true; 
						}
					}
				}
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
		var cellElement = null; 
		var cellsLength = -1;
		var icell = -1; 		
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
				if(_cell.type != "rh" && _cell.childNodes.length > 0 && _cell.childNodes[0].tagName == "TABLE")
				{				
					l = _cell.childNodes[0].cells.length; 
					for(var i = 0; i < l; i++)		
					{						
						if(_cell.childNodes[0].cells[i].getAttribute("type") != "rh")
						{				
							if(toogleCoreIndicator(_cell.childNodes[0].cells[i].childNodes[0].cells, _cell.childNodes[0].cells[i].childNodes[0].cells.length, action))
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
	function toogleRows(rowIndex, rowTable, parentRowID, parentTableID, action, headersToResize,commitAction,expandAll)
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
								inspectedRow.style.display = "block"; 
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
								inspectedRow.style.display = "block";
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
										_offsetRow.style.display = "block"; 
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
			for(var i = 0; i < headersToResize.length; i = i + 2)
			{
				if(resizeTable != null)
				{
					if(headersToResize[i] != resizeTable.getID())
						resizeTable = getGridEX().getTables().getTableByID(headersToResize[i]);			
				}
				else
					resizeTable = getGridEX().getTables().getTableByID(headersToResize[i]);
					
				resizeTable.ResizeHeaderInRow(headersToResize[i+1]); 
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
						getGridEX().MoveNext(); 
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
		setCurrentCell(cell); 
		if(editing != null && editing)
		{
			if(getType() == 11)
				cell.ShowFilterCell(); 
			else
				cell.ShowEditorCell();
		}		
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
			{
				getGridEX().getSelectedItems().SelectSingleRow(gridEXRow); 
				setCurrentCell(getCellByColumnID(getTable().tabCellsOrder[i+1]));
			}
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
		if((getType() == 3 || getType() == 4 || getType() == 9 || getType() == 11) && ((editing != null && editing) || currentCell != null))
			TabCellChanged(editing); 
		else
		{	
			var row = null;
			if(window.event.shiftKey)
				row = getPreviousRow(); 
			else
				row = getNextRow(); 
			if(row != null)
			{		
				getGridEX().setCurrentRow(row); 
				getGridEX().getSelectedItems().SelectSingleRow(row);					
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
				row.TabChanged(editing); 
			}
		}
	}		
	function getColumnSetsCount()
	{
		if(!getTable().getUseColumnSets())
			return 0;
			
		if(columnSetsCount == -1)
		{
			columnSetsCount = 0; 
			var l = innerRow.cells.length;
			for(var i=0;i<l;i++)
			{
				if(innerRow.cells[i].childNodes.length > 0 && innerRow.cells[i].childNodes[0].tagName == "TABLE")
					columnSetsCount++; 
			}
		}
		return columnSetsCount; 
	}	
	function getColumnSetInIndex(columnSetIndex)
	{		
		var columnSet = null; 
		var columnSetOffset = 0; 
		var l = innerRow.cells.length;
		for(var i = 0; i < l; i++)
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
			
				element = element.parentElement; 
			}
			if(element == null)
				throw Error("unable to find root row");
				
			rootInnerRow = element; 
		}		
		return rootInnerRow; 
	}	
	function getInnerItemRow(row)
	{
		return getInnerItemRowCore(row, getGridEX());
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
			{
				if(getType() == 9 && getPosition() == -1 && getTable().getNewRowPosition() == 2)
					nextGridEXRow = getGridEX().getRow(1); 
				else if(getType() == 9 && getPosition() == 0 && getTable().getNewRowPosition() == 2 && getGridEX().getRootTable().getParent() == null && getGridEX().getFilterMode() == 1)
					nextGridEXRow = getGridEX().getRow(2); 
				else
					nextGridEXRow = getGridEX().getRow(getPosition() + 1);
			}
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
				previewInnerRow = innerRow.offsetParent.offsetParent.offsetParent.rows[1]; 
			else
				previewInnerRow = innerRow.offsetParent.rows[innerRow.rowIndex+1]
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
			for(var i = 0; i < l && position == -1; i++)
			{			
				_rootRow = getTable().getHtmlItemsTable().rows[i]; 
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
	this.ResizeHeight = ResizeHeight; 
	this.ResizeHeaderInRow = ResizeHeaderInRow; 		
	this.Unload = Unload; 	
	this.table_onblur = table_onblur;
	this.table_onscroll = table_onscroll;
	setTableDefinition(getGridEX().getTableDefinition());
	this.tabCellsOrder = tabCellsOrder;
	setRowsCss(); 	
	function getAllowAddNew() { return allowAddNew; }	
	function getAllowDelete() { return allowDelete; }	
	function getAllowEdit() { return allowEdit;  }	
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
	function getHierarchicalMode() { return hierarchicalMode; }
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
	function getHeaderWidth() { return (headerWidth == -1) ? 0 : headerWidth; }	
	function getHeaders()
	{
		if(parentTable == null && !getGridEX().isHierarchicalGrid())
			return columnHeaders[0];
		else
			return columnHeaders; 			
	}	
	function getRowHeaders() { return rowheaders; }
	function getSelectorStatus() { return selectorstatus; }
	function setSelectorStatus(value) { selectorstatus = value; }
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
			var i = 0;
			while(i < hiddenColumns.length)
			{
				if(hiddenColumns[i] == rowid)
				{
					var rowhiddencolumns = new Array(); 
					for(var j = i + 1; j < (i + 1) + (hiddenColumnsCount * 2); j++)					
						rowhiddencolumns[rowhiddencolumns.length] = hiddenColumns[j];
					return rowhiddencolumns; 					
				}
				i = i + (hiddenColumnsCount * 2) + 1; 
			}			
		}		
		return null; 		
	}	
	function getID() { return id; }	
	function getKey() { return key; }	
	function getUseColumnSets() { return (usecolumnsets == true || cellLayoutMode == 2); }	
	function getRowCss(rowType)
	{		
		if(rowsCss.length == 0)
			return ""; 
			
		var l = rowsCss.length; 
		for(var i=0; i<l; i=i+2)
		{
			if(rowsCss[i] == rowType)
				return rowsCss[i+1]; 
		}		
		return "";
	}				
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
	function getCellLayoutMode() { return cellLayoutMode; }			
	function getFixedTop()
	{
		var t = 0; 
		t += getGridEX().getPixelTop(); 
		t += divtable.offsetTop;
		if(getHeaders() != null)
		{
			if(getGridEX().isHierarchicalGrid())
				t += getHeaders()[0].getHtmlHeader().offsetHeight; 
			else
				t += getHeaders().getHtmlHeader().offsetHeight; 
		}			
		return t;
	}	
	function getTableHeader()
	{
		if(parentTable == null)		
			return tableHeaders[0]; 
		else
			return tableHeaders;
	}	
	var _divRootHeader = -1; 
	var _divRootNewRecord = -1; 	
	var _divRootFilterRow = -1;
	var _divRootTableHeader = -1;
	function getDivRoot(divType)
	{
		var e = null; 
		var l = getHtmlDiv().childNodes.length; 
		for(var i = 0; i < l; i++)
		{
			e = getHtmlDiv().childNodes[i]; 
			if(e.tagName =="DIV" && e.type != null && parseInt(e.type, 10) == divType)
				return e; 			
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
		if(getGridEX().getGridEXRow() != null)
			getGridEX().getGridEXRow().ShowHeaderIndicator();

		var scrollLeft = table.offsetParent.scrollLeft;
		if(scrollLeft >= 0)
		{	
			var _rtl = getGridEX().getHtmlGridEX().getAttribute("rtl") == "1";			
			if(_divRootHeader == -1)
				_divRootHeader = getDivRoot(1); 
			if(_divRootHeader != null)
			{
				if(_rtl)
				{	
					var x = (table.offsetParent.scrollWidth - table.offsetParent.clientWidth) - table.offsetParent.scrollLeft;
					x *= -1;							
					_divRootHeader.childNodes[0].style.pixelLeft = (x*-1); 
				}				
				else
					_divRootHeader.style.pixelLeft = (scrollLeft * -1); 
			}
			if(_divRootNewRecord == -1)
				_divRootNewRecord = getDivRoot(7);
			if(_divRootNewRecord != null)
			{
				if(_rtl)
				{
					var x = (table.offsetParent.scrollWidth - table.offsetParent.clientWidth) - table.offsetParent.scrollLeft;
					x *= -1;
					_divRootNewRecord.childNodes[0].style.pixelLeft = (x*-1); 
				}
				else
					_divRootNewRecord.style.pixelLeft = (scrollLeft * -1); 
			}
			if(_divRootFilterRow == -1)
				_divRootFilterRow = getDivRoot(9); 
			if(_divRootFilterRow != null)
			{
				if(_rtl)
				{
					var x = (table.offsetParent.scrollWidth - table.offsetParent.clientWidth) - table.offsetParent.scrollLeft;
					x *= -1;
					_divRootFilterRow.childNodes[0].style.pixelLeft = (x*-1); 
				}
				else
					_divRootFilterRow.style.pixelLeft = (scrollLeft * -1); 
			}
			if(_divRootTableHeader == -1)
				_divRootTableHeader = getDivRoot(2);
			if(_divRootTableHeader != null)
			{
				if(_rtl)
				{
					var x = (table.offsetParent.scrollWidth - table.offsetParent.clientWidth) - table.offsetParent.scrollLeft;
					x *= -1;
					_divRootTableHeader.childNodes[0].style.pixelLeft = (x*-1); 
				}
				else
					_divRootTableHeader.style.pixelLeft = (scrollLeft * -1); 
			}
		}
		if(scrollStatus == null)
			scrollStatus = document.getElementById(getGridEX().getID() + "_scrollstatus");
		if(scrollStatus != null)
			scrollStatus.value = table.offsetParent.scrollTop; 
		getGridEX().FireEvent("Scroll", [getGridEX()]);
	}
	var resyncWidth = false; 
	var _minimalSizes = null; 
	function getMinimalColumnSize(colid, colwidth)
	{
		if(_minimalSizes == null)
			_minimalSizes = new Array(); 		
		
		for(var i=0; i<_minimalSizes.length; i=i + 2)
		{
			if(_minimalSizes[i] == colid)
			{
				if(resyncWidth && _minimalSizes[i + 1] != colwidth)
				{
					_minimalSizes[i+1] = colwidth; 
					resyncWidth = false; 
				}						
				return _minimalSizes[i + 1]; 
			}				
		}			
		_minimalSizes[_minimalSizes.length] = colid;
		_minimalSizes[_minimalSizes.length] = colwidth;
		return colwidth;		
	}
	function AutoSizeColumn(column)
	{
		if(usecolheaders)
			columnHeaders[0].ColumnAutoSize(column,null); 
	}
	function AutoSizeExpandColumn(cell)
	{
		var columnid = getColumnIDFromCellID(cell.id); 
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
				// AutoSizeColumnSetCells(cell, maxColumnSize); 
			}
			else
				AutoSizeCells(cell, maxColumnSize); 
		}
	}
	function AutoSizeCells(column, width)
	{
		var _col = null;
		var _fullAutoSize = false; 
		var _itemsTables = document.getElementsByName(getID() + "_i"); 
		var l = _itemsTables.length; 
		if(l > 0)
		{
			for(var i=0; i<l; i++)
			{
				_itemsCols = _itemsTables[i].getElementsByTagName("COL"); 
				if(i == 0 && getGridEX().getColumnAutoResize())
					_fullAutoSize = _itemsCols.length > 1;				
				_col = _itemsCols[column.cellIndex];
				if(getGridEX().getColumnAutoResize() && !_fullAutoSize)
					width = getGridEX().getResizeWidth(); 
				_col.width = width + "px";
			}			
			if(getGridEX().getColumnAutoResize() && _fullAutoSize)
				AutoSizeByCells(); 
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
					
				var z = _firstRecord.cells.length; 
				for(var i = 0; i < z; i++)
				{
					var cell = _firstRecord.cells[i]; 
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
	function AutoSizeByCells()
	{		
		getFirstRecord();
		if(_firstRecord == null)
		{
			var _table = getGridEX().getRootTable().getHtmlItemsTable(); 
			if(_table.getAttribute("empty") != null)
			{
				_table.style.pixelWidth = getGridEX().getResizeWidth(); 
				var _cols = _table.getElementsByTagName("COL"); 
				for(var i=0;i<_cols.length;i++)
					_cols[i].width = (getGridEX().getResizeWidth() / _cols.length) + "px"; 
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
			var _headerwidth = 0; 
			for(var icolumnset = 0; icolumnset < _columnsetcellsLength; icolumnset++)
			{
				_tmpcell = _firstRecord.cells[icolumnset];
				if(_tmpcell.type != "rh")
				{					
					originalsets[originalsets.length] = _tmpcell.offsetWidth; 
					_columnsettable = _tmpcell.childNodes[0]; 
					_cellslength = _columnsettable.cells.length; 
					var _fixedwidth = new Array(_columnsettable.getElementsByTagName("COL").length);
					for(var _icell = 0; _icell < _cellslength; _icell++)
					{					
						cell = _columnsettable.cells[_icell];
						if(cell.id != null && cell.id != "")
						{
							_colid = getColumnIDFromCellID(cell.id); 
							if(_colid != "" && !getGridEXColumnByClientID(_colid).getAllowSize())							
								_fixedwidth[cell.usecol] = cell.offsetWidth; 																						
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
				else
					_headerwidth = _tmpcell.offsetWidth; 				
			}
			var _newwidth = getGridEX().getResizeWidth() - (_headerwidth + _hierarchicalwidth); 
			_oldwidth -= _headerwidth; 
			var _gridexcolumn = null; 
			var _newcolumnsetsize = null;
			var _newcolumnsets = new Array(); 
			var newcolumnsetwidth = -1; 
			var actualwidth = 0;
			var icolumnset = 0; 
			for(var j = 0; j < _columnsetcellsLength; j++)
			{
				_tmpcell = _firstRecord.cells[j]; 
				if(_tmpcell.type != "rh")
				{	
					newcolumnsetwidth = Math.round((originalsets[icolumnset] * _newwidth) / _oldwidth); 
					_columnsettable = _tmpcell.childNodes[0]; 
					_cellslength = _columnsettable.cells.length; 
					_cols = _columnsettable.getElementsByTagName("COL"); 
					var _newcellswidth = new Array(_cols.length); 
					for(var _icell = 0; _icell < _cellslength; _icell++)
					{
						cell = _columnsettable.cells[_icell]; 
						if(cell.id != null && cell.id != "")
						{
							_colid = getColumnIDFromCellID(cell.id); 
							_gridexcolumn = getGridEXColumnByClientID(_colid); 
							if(_gridexcolumn.getAllowSize())
							{							
								_newcellwidth = Math.round((getPixelColWidth(_cols[_gridexcolumn.getColumnSetColumn()].width) * (newcolumnsetwidth - _fixedcolumnsets[icolumnset])) / (originalsets[icolumnset] - _fixedcolumnsets[icolumnset])); 
								_newcellswidth[_gridexcolumn.getColumnSetColumn()] = _newcellwidth;
							}
						}
					}
					for(var i = 0; i < _newcellswidth.length; i++)
					{
						if(_newcellswidth[i] != null)
							actualwidth += _newcellswidth[i]; 
					}
					_newcolumnsets[_newcolumnsets.length] = _newcellswidth; 
					icolumnset++;
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
						for(var i = 0; i < _newcolumnsetsize.length && diffsize != 0; i++)
						{
							if(_newcolumnsetsize[i] + offset > 0)
							{
								_newcolumnsetsize[i] += offset; 
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
				 _itemsTables = document.getElementsByName(getID() + "_items_cs" + icolumnset); 
				 _itemsTablesLength = _itemsTables.length; 
				 for(var _item = 0; _item < _itemsTablesLength; _item++)
				 {
					_itemsCols = _itemsTables[_item].getElementsByTagName("COL"); 
					_newcolumnsetsize = _newcolumnsets[icolumnset]; 
					for(var i = 0; i < _newcolumnsetsize.length; i++)
					{
						if(_newcolumnsetsize[i] != null && (_newcolumnsetsize[i] != getPixelColWidth(_itemsCols[i].width)))
							_itemsCols[i].width = _newcolumnsetsize[i] + "px"; 
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
			for(var i=0; i<_cellsLength; i++)
			{
				cell = _firstRecord.cells[i];
				if(cell.type != "rh")
				{
					_colid = getColumnIDFromCellID(cell.id);
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
			for(var i = 0; i < _cellsLength; i++)
			{
				cell = _firstRecord.cells[i]; 
				if(cell.type != "rh")
				{
					_colid = getColumnIDFromCellID(cell.id); 
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
					for(var i = 0; i < newsizeslength && (diffsize != 0 && countzero != cellsresize); i = i + 2)
					{
						if(newsizes[i+1] + offset > 0)
							newsizes[i+1] += offset; 
						else
							countzero++; 
							
						actualwidth += offset;
						diffsize = newwidth - actualwidth; 
					}
				} while(diffsize != 0 && countzero != cellsresize);
			}
			var _itemsTables = document.getElementsByName(getID() + "_i"); 
			var _itemsTablesLength = _itemsTables.length; 
			if(_itemsTablesLength > 0)
			{
				for(var i = 0; i < _itemsTablesLength; i++)
				{
					_itemsCols = _itemsTables[i].getElementsByTagName("COL"); 
					for(var _index = 0; _index < newsizeslength; _index = _index + 2)
					{
						_col = _itemsCols[newsizes[_index]]; 
						_col.width = newsizes[_index+1] + "px"; 
					}
				}				
			}
		}
	}	
	function AutoSizeColumns() { AutoSizeByColumns(); }	
	function FixedResize()
	{
		if(getParent() != null)
			throw Error("invalid method operation call"); 
			
		ResizeHeight(); 
	}	
	function IsParentOf(childID)
	{
		if(childTablesArray == null)
			return false; 
			
		for(var i=0; i<childTablesArray.length; i++)
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
			var _ch = null; 
			var l = columnHeaders.length; 
			for(var i=0; i<l; i++)
			{
				_ch = columnHeaders[i]; 
				if(_ch.getRowIndex() == rowIndex)
				{
					_ch.AutoSizeColumnsAfterDisplay(); 
					return;
				}
			}
		}
	}	
	function ResizeHeight()	
	{	
		if(getParent() == null) 
		{			
			var divgridex = getGridEX().getHtmlGridEX();
			var gridexparent = getGridEXOffsetParent(divgridex); 		
			if(gridexparent != null)
			{											
				var clientheight = gridexparent.clientHeight; 						
				var offset = getBorderStyleWidth(divgridex.style.borderTopWidth) + getBorderStyleWidth(divgridex.style.borderBottomWidth) + divgridex.style.pixelTop; 
				offset += (getBottomOffset(gridexparent) + getTopOffset(gridexparent));															
				if(divgridex.style.height != null && divgridex.style.height.indexOf("%") >= 0)
					clientheight = clientheight * (getPercentWidth(divgridex.style.height) / 100);				
				
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
				else if(obj != null && obj.length > 0 &&  getGridEX().isHierarchicalGrid())
					clientheight -= obj[0].getHtmlHeader().offsetHeight;				
					
				clientheight -= newRecordHeight;
				clientheight -= filterRowHeight; 
				clientheight -= separatorHeight; 
				clientheight -= tableHeaderHeight;
				clientheight -= pagerNavigatorHeight;					
				if(clientheight <= 0)
					return;
					
				table.offsetParent.style.pixelHeight = clientheight; 								
			}
		}
		else
			throw Error("invalid operation call"); 
	}
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
				if(_tmpRow.type == null || (parseInt(_tmpRow.type, 10) == 3 || parseInt(_tmpRow.type, 10) == 4))
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
							for(var i = 0; i < _innerCellsLength; i++)
							{
								if(_innerRow.cells[i].childNodes[0].tagName == "TABLE")
								{	
									if(_innerRow.cells[i].childNodes[0].rows[0].value != null && _innerRow.cells[i].childNodes[0].rows[0].value == value)
										return [_tmpRow, _innerRow.cells[i].childNodes[0].rows[0]];
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
							if(_tmpRow.value != null && _tmpRow.value == value)
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
				if(_tmpRow.type == null || (parseInt(_tmpRow.type, 10) == 3 || parseInt(_tmpRow.type, 10) == 4))
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
				if(_tmpRow.type == null || (parseInt(_tmpRow.type, 10) == 3 || parseInt(_tmpRow.type, 10) == 4))
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
							for(var i = 0; i < _innerCellsLength; i++)
							{
								if(_innerRow.cells[i].childNodes[0].tagName == "TABLE")
								{	
									_value = _innerRow.cells[i].childNodes[0].rows[0].getAttribute("value");
									_display = _innerRow.cells[i].childNodes[0].rows[0].getAttribute("displayMember"); 									
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
		for(var i=0; i<l; i++)
		{
			r = columnHeaders[i].HitTestColumns(x, y);
			if(r != null)
				return r; 
		}		
		return null; 
	}	
	function Unload()
	{	
		unloadArray(tableHeaders); 
		unloadObjectsArray(columnHeaders); 
		delete columnHeaders;
		columnHeaders = null;
		unloadArray(rowsCss); 
		unloadArray(childTablesArray);
		if(columnsCollection != null)
			columnsCollection.Unload();
		delete columnsCollection;
		columnsCollection = null;
		unloadObjectsArray(tableHeaders);
		delete tabCellsOrder;
		tabCellsOrder = null; 
		delete tableHeaders;
		tableHeaders = null;
		columnHeaders = null;
		rowsCss = null;
		childTablesArray = null;				
		_firstRecord = null;
		divtable = null;	
		if(table != null)
		{						
			table.detachEvent("onmousemove", table_onmousemove);
			table.detachEvent("onselectstart", table_onselectstart);
			table.offsetParent.detachEvent("onblur", gtable_onblur);
			table.offsetParent.detachEvent("onscroll", gtable_onscroll);
			table.offsetParent.removeAttribute("table");
		}
		table = null;		
		parentTable = null;
		gridEX = null; 
		_divRootHeader = null;
		_divRootNewRecord = null;
		_divRootFilterRow = null;
		_gridEXTable = null;
	}
	function setTableDefinition(definitions)
	{	
		var DEFINITION_LENGTH = 20;	
		for(var i = 0; i < definitions.length; i += DEFINITION_LENGTH)
		{
			if(definitions[i] == id)
			{	
				key = definitions[i+1]; 
				hierarchicalMode = definitions[i+2];
				isParentTable = (definitions[i+3] == 1) ? true : false;		
				childTablesArray = (definitions[i+4] != null && definitions[i+4].length > 0) ? (definitions[i+4]).split("$") : null; 
				rowheaders = (definitions[i+5] == 1) ? true : false;
				usecolheaders = (definitions[i+6] == 1) ? true : false;
				cellLayoutMode = definitions[i+7];
				headerType = definitions[i+8];
				headerWidth = definitions[i+9]; 
				allowAddNew = (definitions[i+10] == 1) ? true : false;
				newRowPosition = definitions[i+11]; 
				allowEdit = (definitions[i+12] == 1) ? true : false;
				allowDelete = (definitions[i+13] == 1) ?  true : false; 
				previewRow = (definitions[i+14] == 1) ? true : false;
				expandedPreviewRowGlyph = definitions[i+15]; 
				collapsedPreviewRowGlyph = definitions[i+16];
				selectorstatus = (definitions[i+17] == 1) ? true : false;
				autosizeexpandcolumn = (definitions[i+18] != null && definitions[i+18].length > 0) ? definitions[i+18] : null;
				tabCellsOrder = definitions[i+19];
				return; 
			}
		}
	}	
	function setRowsCss() { rowsCss = eval(getID() + "_client_rows_css"); }	
	function getTableColInPosition(position)      
	{
		var cols = table.all.tags("COL");
		for(var i=0; i<cols.length; i++)
		{
			if(cols[i].pos != null)
			{
				if(parseInt(cols[i].pos, 10) == position)
					return cols[i]; 
			}
		}
	}
	function recordsTable(htmltable)
	{		
		if(htmltable != null)
		{
			var divs = htmltable.all.tags("DIV");			
			if(divs != null && divs.length > 0)
			{
				var _div = null; 
				var l = divs.length; 
				for(var i = 0; i < l; i++)
				{					
					_div = divs[i]; 
					if(_div.type != null && parseInt(_div.type, 10) == 4)
						return _div.all.tags("TABLE")[0];						
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
			for(var i = rowPos; i < _rows.length; i++)
			{				
				if(childTables != null && childTables.Count() == childTablesArray.length)
					return; 
			
				_row = _rows[i];
				if(_row.pt != null && _row.pt == id && _row.t != _lastid)
				{															
					if(childTables == null)
						childTables = new GridEXChildTableCollection(gridEXTable);							
					if(childTables.getTableByID(_row.t) == null)
					{
						childTables.Add(new GridEXTable(null, _row.t, gridEXTable, _row.rowIndex, null));
						_lastid = _row.t; 
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
			var htmlheader = divheader.childNodes[0];
			if(htmlheader != null)
			{					
				var columnsets = new GridEXColumnSetCollection(gridEXTable, htmlheader, true, gridEXHeader); 
				var row = htmlheader.rows[0];				
				var l = row.cells.length; 
				var cell = null;
				var columnset = null; 
				for(var i=0; i<l; i++)
				{
					cell = row.cells[i]; 
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
			var divs = htmldiv.all.tags("DIV");
			var l = divs.length; 
			if(divs != null && l > 0)
			{
				var _div = null; 				
				var columnsheader = null; 
				for(var i = 0; i <  l; i++)
				{	
					_div = divs[i]; 
					if(_div.type != null && parseInt(_div.type, 10) == 1)
					{
						columnsheader = new GridEXColumnHeaders(gridEXTable, null, _div.all.tags("TABLE")[0], headerType, 0, true); 
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
		var l = rootTable.rows.length; 
		var _row = null; 
		for(var i = rowPos; i < l; i++)
		{
			_row = rootTable.rows[i]; 
			if( _row.type == "1" && _row.t  != null && _row.t == gridEXTable.getID()) 
			{				
				columnsheader = new GridEXColumnHeaders(gridEXTable, _row, _row.cells[0].getElementsByTagName("DIV")[0].getElementsByTagName("TABLE")[0] , headerType, _pos);
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
			var divs = htmltable.all.tags("DIV");
			if(divs != null && divs.length > 0)
			{
				var _div = null; 
				for(var i = 0; i < divs.length; i++)
				{
					_div = divs[i]; 
					if(_div.type != null && parseInt(_div.type, 10) == 2)
					{
						if(tableHeaders == null)
							tableHeaders = new Array();
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
			if(_row.pt != null && _row.pt == gridEXTable.getID() && _row.type != null && parseInt(_row.type, 10) == 2)
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
			var _columnSets = getColumnSetsFromHeader(this, columnHeaders[0], columnHeaders[0].getHtmlHeader()); 
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
			getChildrenColumnHeader(this, 0, getHtmlItemsTable(), headerType, columnHeaders);  
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
		if(usecolheaders)
			getChildrenColumnHeader(this, rowpos, getHtmlItemsTable(), headerType, columnHeaders);
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
	if(getGridEX().isHierarchicalGrid())
		setChildTables(this, rowpos);
	if(parentTable == null && divtable != null)
	{
		if(getGridEX().getHtmlGridEX().all.tags("DIV").length > 0)
		{			
			var _pagerdiv = getGridEX().getHtmlGridEX().all.tags("DIV")[0];
			if(parseInt(_pagerdiv.type, 10) == 6)
				pagerNavigatorHeight += _pagerdiv.offsetHeight; 
		}
		var divs = divtable.all.tags("DIV");
		var _div = null;
		var l = divs.length; 
		for(var i = 0; i < l; i++)
		{
			_div = divs[i];
			if(parseInt(_div.type, 10) == 6)
				pagerNavigatorHeight += _div.offsetHeight;
			else if(parseInt(_div.type, 10) == 7)
				newRecordHeight += _div.offsetHeight; 
			else if(parseInt(_div.type, 10) == 8 && separatorHeight == 0)
				separatorHeight += _div.offsetHeight;
			else if(parseInt(_div.type, 10) == 9)
				filterRowHeight += _div.offsetHeight; 				
			else if(parseInt(_div.type, 10) == 2 && _div.parentElement == divtable)
				tableHeaderHeight += _div.offsetHeight;
		}
		divs = null;
	}
	if(table != null)
	{
		table.attachEvent("onmousemove", table_onmousemove);
		table.attachEvent("onselectstart", table_onselectstart); 									
		table.offsetParent.attachEvent("onblur", gtable_onblur); 
		table.offsetParent.attachEvent("onscroll", gtable_onscroll);						
		table.offsetParent.setAttribute("table", this); 
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
	var selectionMode = 1;	
	var selectonexpand = true; 	
	var useHiddenColumns = false; 		
	var owner = null; 	
	var hiddenValuesCollection = hiddenValues; 
	var hitArea = -1; 
	var selectedItemsCollection = null;	
	var tableDefinition = tablesDefinition;	
	var arrRowsCss = rowsCss;	
	var controlsToBuild = null; 
	var editControls = null;	
	var resetFilterCommand = null; 	
	var htmlGridEX = document.getElementById(id);  
	if(htmlGridEX == null)
		throw Error("DIV object with ID '" + id + "' is null or invalid"); 
		
	if(htmlGridEX.name == null || htmlGridEX.name == "")
		throw Error("unable to find server id attribute on DIV");
		
	this.currentCell = null;
	this.focusCss = ""; 
	this.focusRowCss = "";
	this.ddbimg = ""; 
	this.ddbpimg = ""; 
	this.selpb = false; 
	this.ddpb = false; 
	this.cmpb = false; 
	var isfiltered = false; 
	var htmlGridEXParent = null; 		
	var serverid = htmlGridEX.name;			
	var childLoadingMode = 1;
	var currentEditAction = -1; 		
	var filterMode = -1; 
	var haltEdition = false;	
	var gridEXRow = null;
	var gridEXRows = null;
	var initRowID = null; 		
	var resizeGroups = false; 
	var resizeHeight = false;
	var resizeWidth = false;
	var resizeMode = -1; 
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
	var recordExpandGlyph = ""; 
	var recordCollapseGlyph = ""; 
	var groupExpandGlyph = ""; 
	var groupCollapseGlyph = ""; 		
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
	this.getResetFilterCommand = getResetFilterCommand; 
	this.getID = getID; 	
	this.getIsInitialized = getIsInitialized; 
	this.getSelectOnExpand = getSelectOnExpand; 
	this.getSelectedClassName = getSelectedClassName;
	this.getGroupByBox = getGroupByBox;	
	this.getRootTable = getRootTable;
	this.getColumnAutoResize = getColumnAutoResize; 
	this.getFixTableSize = getFixTableSize; 
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
	this.getRow = getRow; 	
	this.getRowInIndex = getRowInIndex; 
	this.getRowByID = getRowByID; 
	this.getRowsInPageCount = getRowsInPageCount; 
	this.getRowHeaderEditGlyph = getRowHeaderEditGlyph; 
	this.getRowHeaderGlyph = getRowHeaderGlyph; 	
	this.getSelectOnExpand =getSelectOnExpand; 
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
	this.setCurrentRow = setCurrentRow; 	
	this.setOwner = setOwner; 	
	this.setHitTestArea = setHitTestArea; 		
	this.TabElementChanging = TabElementChanging; 	
	this.body_onselectstart = body_onselectstart;
	this.gridEX_onblur = gridEX_onblur;	
	this.gridEX_onkeydown = gridEX_onkeydown; 			
	this.gridEX_onresize = gridEX_onresize;		
	this.gridEX_onload = gridEX_onload;
	this.gridEX_onsubmit = gridEX_onsubmit;
	this.gridEX_onunload = gridEX_onunload;
	this.gridEX_onmousewheel = gridEX_onmousewheel;
	if(clientDefinition != null)
	{
		rowcount = clientDefinition[0]; 
		recordcount = clientDefinition[1]; 
		isfiltered = (clientDefinition[2] == 1) ? true : false; 
		columnAutoResize = (clientDefinition[3] == 1) ? true : false; 		
		resizeHeight = (clientDefinition[4] == 1) ? true : false;
		resizeWidth = (clientDefinition[5] == 1) ? true : false;
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
		resizeGroups = (clientDefinition[12] == 1) ? true: false;
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
		this.focusCss = (clientDefinition[25] == null) ? null : clientDefinition[25][0]; 
		this.focusRowCss = (clientDefinition[25] == null) ? null : clientDefinition[25][1]; 
		unloadArray(clientDefinition); 
	}	
	if(columnAutoResize)
		htmlGridEXParent = getGridEXOffsetParent(htmlGridEX); 
	
	function getRowByID(rowID)
	{
		if(gridEXRows == null)
			return null; 
		
		var l = gridEXRows.length; 
		var r = null; 			
		for(var i=0; i<l; i++)
		{		
			r = gridEXRows[i]; 
			if(r.getID() == rowID)
				return r; 
		}				
		return null; 
	}	
	function AutoSizeColumns()
	{	  
		if(rootTable != null)	
		{
			rootTable.AutoSizeColumns();
			var i = document.getElementById(getID() + "_clientwidth");
			i.value = getHtmlWidth(); 			
		}
	}	
	function DoPostBack(eventTarget, eventArgument, updateData)
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
			var j = 0; 
			var _hiddenValues = null; 
			var l = hiddenValuesCollection.length; 
			var i = 0; 
			var _table = null; 
			while(i < l)
			{
				if(hiddenValuesCollection[i] == row.getID())
				{
					if(_table == null || _table.getID() != hiddenValuesCollection[i+1])
						_table = getTables().getTableByID(hiddenValuesCollection[i+1]);						
					
					_hiddenValues = new Array(); 					
					for(var j = i + 2; j <= (i + _table.getColumns().HiddenColumnsCount() * 2); j = j + 2)
					{
						_hiddenValues[_hiddenValues.length] = hiddenValuesCollection[j]; 
						_hiddenValues[_hiddenValues.length] = hiddenValuesCollection[j+1];						
					}								
					return _hiddenValues; 
				}
				else
				{
					if(_table == null || _table.getID() != hiddenValuesCollection[i+1])
						_table = getTables().getTableByID(hiddenValuesCollection[i+1]);
						
					i += (_table.getColumns().HiddenColumnsCount() * 2) + 1;
					i++; 
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
	function CancelCurrentActions() { cancelCurrentUIEvents(); }
	function FireEvent(eventname, eventparams)
	{	
		if(eventhandlers == null || eventhandlers.length == 0)
			return null;
			
		var l = eventhandlers.length; 
		var e = "";		
		for(var i=0; i<l; i=i+2)
		{
			if(eventhandlers[i] == eventname) 
			{
				e = eventhandlers[i+1];
				i = l; 
			}
		}
		if(e != "")
		{	
			var p = "";
			var l = eventparams.length; 
			for(var i = 0; i < l; i++)
			{
				if(p != "") p += ",";
				p += "eventparams[" + i + "]"; 
			}		
			var c = "return eval(" + e + "("  + p + "))";
			var f = new Function("eventparams", c);
			return f(eventparams);
		}
		return null; 
	}					
	function FilterData()
	{
		var xml = getFilterXML(getFilterRow());  
		if(xml.length > 0)
		{
			var _input = document.getElementById(getID() + "_editinfo");
			if(_input == null)
				throw Error("unable to find edit info field"); 
				
			_input.value = xml;			
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
			var c = record.getCellByIndex(i);
			if(c.getDataChanged() && c.getColumn().isGrouped)
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
		
		if(_tableID != null && getTables().getTableByID(_tableID).getUseColumnSets())
		{
			if(_innerItemRow == null)
				_innerItemRow = row;
				
			if(_innerItemRow.type == null || _innerItemRow.type == "3" || _innerItemRow.type == "4" || _innerItemRow.type == "9")				
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
	function MoveNextFocusCell() { getGridEXRow().NextFocusCell(); }
	function MovePreviousFocusCell() { getGridEXRow().PreviousFocusCell(); }
	function moveToPreviousFocusCell(row)
	{
		if(row.getRowType() == "Record" || row.getRowType() == "NewRecord" || row.getRowType() == "FilterRow")
		{
			if(row.getTable().tabCellsOrder != null && row.getTable().tabCellsOrder.length > 0)
				row.setCurrentCell(row.getCellByColumnID(row.getTable().tabCellsOrder[row.getTable().tabCellsOrder.length-1]));
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
			if(tmpRow.type != "1" && tmpRow.type != "2" && tmpRow.type != "6" && tmpRow.type != "7" && tmpRow.type != "10")
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
			if(tmpRow.type != "1" && tmpRow.type != "2" && tmpRow.type != "6" && tmpRow.type != "7" && tmpRow.type != "10")
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
	function isDropDown() { return isdropdown; }
	function isFiltered() { return isfiltered; }
	function isVisible() { return (htmlGridEX.currentStyle.visibility != "hidden" && htmlGridEX.currentStyle.display != "none" && htmlGridEX.offsetWidth > 0); }		
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
	function getColumnAutoResize() { return columnAutoResize; }	
	function getColumnSetNavigation() { return columnSetNavigation; }	
	function getChildLoadingMode() { return childLoadingMode; }	
	function getEditControl(controlType, controlID)
	{		
		if(editControls == null || editControls.length == 0)
			throw Error("editControls collection is null or invalid"); 
			
		var c = null; 
		var l = editControls.length; 
		for(var i = 0; i < l; i = i + 2)
		{			
			if(editControls[i] == controlType)
			{
				c = editControls[i+1]; 
				if(controlID != null)
				{				
					if(c.getID != null && c.getID() == controlID)
						return c; 
				}				
				else			
					return c; 
			}
		}		
		throw Error("argument out of range"); 
	}	
	function getFilterMode() { return filterMode; }	
	function getFilterRow()
	{	
		if(gridEXRows == null)
			return null; 
			
		var l = gridEXRows.length;
		for(var i = 0; i < l; i++)
		{		
			if(gridEXRows[i].getType() == 11)
				return gridEXRows[i]; 
		}		
		return getRowFromTop(9); 
	}	
	function getFixTableSize() { return fixTableSize; }
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
			var l = gridEXRows.length;
			var tmprow = null; 
			for(var i=0; i<l; i++)
			{
				tmprow = gridEXRows[i]; 
				if(tmprow.getType() == 9 && tmprow.getTable().getParent() == null)
					return tmprow;
			}			
		}
		return getRowFromTop(7); 
	}	
	function getRowFromTop(rowType)
	{	
		var d = null;
		var _divs = getRootTable().getHtmlDiv().all.tags("DIV"); 
		var l = _divs.length; 
		for(var i=0; i<l; i++)
		{
			d = _divs[i]; 
			if(d.type != null && parseInt(d.type, 10) == rowType)
			{
				if(d.childNodes[0].tagName == "TABLE")
				{
					if(d.childNodes[0].rows.length > 0)
						return RetrieveRow(d.childNodes[0].rows[0], getInnerItemRow(d.childNodes[0].rows[0]), getRootTable(), 0);
				}
			}
		}
		return null; 
	}	
	function getRowHeaderGlyph() { return document.getElementById(getID() + "_rowheaders_glyph"); }	
	function getRowHeaderEditGlyph() { return document.getElementById(getID() + "_rowheaders_edit_glyph"); }
	function isHierarchicalGrid() { return hierarchicalGrid; }	
	function getID() { return id; }	
	function getIsInitialized() { return initialized; }	
	function getResizeGroups() { return resizeGroups; }
	function getServerID() { return serverid; }	
	function getGroupByBox() { return groupByBox; }	
	function getPixelTop()
	{
		var t = 0; 
		var p = htmlGridEX;				
		while(p != null)
		{
			t += p.offsetTop; 
			p= p.offsetParent; 
		}
		if(htmlGridEX.offsetParent.scrollTop != 0)
			t -= htmlGridEX.offsetParent.scrollTop; 
		return t;
	}	
	function getPixelLeft()
	{
		var l = 0;
		var p = htmlGridEX;
		while(p != null)
		{
			l += p.offsetLeft;
			p = p.offsetParent; 
		}
		return l; 
	}	
	function getRootTable() { return rootTable; }	
	function getSelectOnExpand() { return selectonexpand; }
	function getGridEXRow() { return gridEXRow; }	
	function getNextVisibleRow(i, rowPosition)
	{	
		var _innerRow = null; 
		var _innerType = null; 
		var l = getRootTable().getHtmlItemsTable().rows.length; 
		while(i < l)
		{
			_innerRow = getRootTable().getHtmlItemsTable().rows[i]; 
			_innerType = _innerRow.getAttribute("type"); 
			if(_innerType != "1" && _innerType != "2" && _innerType != "6" && _innerType != "7" && _innerType != "10")
			{
				if(_innerRow.style.display != "none")
					return RetrieveRow(_innerRow, getInnerItemRow(_innerRow), null, rowPosition); 
				else
					rowPosition++; 
			}
			i++; 
		}
		return null; 
	}	
	function getPreviousVisibleRow(i, rowPosition)
	{			
		var _innerRow = null; 				
		if(i < 0) 
		{	
			if(rowPosition == 0 && getFilterMode() == 1)
				return getFilterRow();
			else if(rowPosition == 0 && getRootTable().getParent() == null && getRootTable().getNewRowPosition() == 2)
				return getNewRecord(); 				
			else if(rowPosition == 1 && getFilterMode() == 1 && getRootTable().getParent() == null && getRootTable().getNewRowPosition() == 2)
				return getNewRecord();							
		}		
		var _innerType = null; 
		while(i >= 0)
		{			
			_innerRow = getRootTable().getHtmlItemsTable().rows[i]; 
			_innerType = _innerRow.getAttribute("type"); 
			if(_innerType != "1" && _innerRow.getAttribute("type") != "2" && _innerType != "6" && _innerType != "7" && _innerType != "10")
			{
				if(_innerRow.style.display != "none")
					return RetrieveRow(_innerRow, getInnerItemRow(_innerRow), null, rowPosition); 
				else
					rowPosition--; 
			}
			i--; 
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
			var r = null; 
			rowslength = gridEXRows.length; 
			for(var i=0; i<rowslength; i++)
			{				
				r = gridEXRows[i]; 
				if(r.getPosition() == position)
					return r;
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
		for(var i = 0; i < rowslength; i++)
		{
			_innerRow = getRootTable().getHtmlItemsTable().rows[i]; 
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
	function getRowsInPageCount() { return rowcount; }	
	function getRecordCount() { return recordcount; }
	function getSelectionMode() { return selectionMode; }	
	function getThemedAreas() { return 1; }	
	function getUseHiddenColumns() { return useHiddenColumns; }		
	function getClassName(rowID)
	{		
		var l = arrRowsCss.length; 
		for(var i=0; i<l; i=i+3)
		{			
			if(arrRowsCss[i] == rowID)
				return arrRowsCss[i+1]; 
		}		
		return null; 
	}	
	function getSelectedClassName(rowID)
	{
		var l = arrRowsCss.length; 
		for(var i=0; i<l; i=i+3)
		{
			if(arrRowsCss[i] == rowID)
				return arrRowsCss[i+2]; 
		}		
		return null; 
	}	
	function getSelectedItems() { return selectedItemsCollection; }	
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
	function getTableDefinition() { return tableDefinition; }	
	function getUpdateMode() { return updateMode; }	
	function getUpdateOnLeave() { return updateOnLeave;  }		
	function setOwner(value) { owner = value; }	
	function getFilterXML(_gridEXRow)
	{
		if(_gridEXRow == null)
			return; 
			
		if(_gridEXRow.getType() != 11)
			throw Error("invalid operation exception: row is not a filter row")
			
		var _editInnerXML = ""
		var _editOuterXML = ""
		var _cell = null;
		var l = _gridEXRow.getCellsLength(); 
		for(var i = 0; i < l; i++)
		{
			_cell = _gridEXRow.getCellByIndex(i); 
			if(_cell.getDataChanged() || _cell.getText() != "") 
			{
				if(_cell.getValue() == null || _cell.getText() == "" || _cell.getInnerCell().getAttribute("remfilter") != null)
					_editInnerXML += "[tag:]cell id='" +  _cell.getColumn().getClientID() + "' action='-1' /[:tag]";
				else
				{	
					if(_cell.getInnerCell().getAttribute("niv") != null)
						_editInnerXML += "[tag:]cell id='"+_cell.getColumn().getClientID()+"' niv='1'[:tag]"+encodeURI(_cell.getInnerCell().getAttribute("niv"))+"[tag:]/cell[:tag]";
					else
					{
						_editInnerXML += "[tag:]cell ";
						_editInnerXML += "id='"+_cell.getColumn().getClientID()+"'"; 					
						_editInnerXML += "[:tag]";				
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
				if((_cell.getDataChanged() || (_cell.getInnerCell() != null && _cell.getInnerCell().getAttribute("ind") != null)) && !_cell.getColumn().getActAsSelector())
				{
					if(_cell.getInnerCell() == null || _cell.getInnerCell().getAttribute("niv") == null)
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
			var _input = document.getElementById(getID() + "_editinfo");
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
			var _input = document.getElementById(getID() + "_editinfo"); 
			if(_input == null)
				throw Errow("unable to find edit info field"); 
				
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
			
		var xml = getEditableXML(currentEditAction, null);
		if(xml.length > 0)
		{
			var input = document.getElementById(getID() + "_editinfo");
			if(input == null)
				throw Error("unable to find edit info field"); 
				
			input.value = xml;
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
			var batchXML = ""; 
			var isbatch = false;
			if(getUpdateMode() == 2)
			{
				batchXML = ResumeEditBatchRecordCore();
				if(batchXML.length > 0)
					isbatch = true;					
			}
			if(isbatch)
				xml = "[tag:]rows action='2'[:tag]" + batchXML + xmlitems + "[tag:]/rows[:tag]"; 
			else
				xml = "[tag:]rows action='3'[:tag]" + xmlitems + "[tag:]/rows[:tag]";		
			var _input = document.getElementById(getID() + "_editinfo"); 
			if(_input == null)
				throw Error("unable to find edit info field"); 
			
			_input.value = xml; 
			DoPostBack(null, "ResumeEditing");
			window.event.returnValue = false;
			window.event.cancelBubble = true;
		}
		else
		{
			
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
	function ResumeFilterOperation() { FilterData(); }	
	function isEditing() { return (currentEditAction != -1 && gridEXRow != null);  }	
	function isFiltering() { return (gridEXRow != null && gridEXRow.getType()  == 11); }	
	function setCurrentRow(row, selectRow)
	{		
		if(isEditing()) 
		{
			if(getGridEXRow() != row)
			{
				if(ResumeEditOperation())
				{
					if(gridEXRow != null)
						gridEXRow.HideHeaderIndicator();
					gridEXRow = row; 				
					if(gridEXRow != null)
						gridEXRow.ShowHeaderIndicator();		
					return; 
				}
			}
		}
		else if(isFiltering())
		{
			if(getGridEXRow() != row && getGridEXRow().getDataChanged())
			{
				ResumeFilterOperation();
				if(gridEXRow != null)
					gridEXRow.HideHeaderIndicator();
				gridEXRow = row; 	
				if(gridEXRow != null)
					gridEXRow.ShowHeaderIndicator();
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
			if(gridEX.currentCell != null)
			{
				if(gridEX.currentCell.getInnerCell().getAttribute("align") == "center" || gridEX.currentCell.getInnerCell().getAttribute("align") == "right" || gridEX.currentCell.getInnerCell().style.textAlign != "")
					gridEX.currentCell.getInnerCell().style.textAlign = "";
				gridEX.currentCell.getInnerCell().setAttribute("className", gridEX.currentCell.cssName);
			}
		}		
		if((gridEXRow != row && getSelectionMode() != 3) || (gridEXRow != null && gridEXRow == row && !gridEXRow.getSelected() ))
		{
			if(selectRow == null || selectRow)
				FireEvent("SelectionChanged", [row]);
		}
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
			var l = gridEXRow.getCellsLength();
			for(var i=0;i<l;i++)
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
			
		if(document.activeElement != null && document.activeElement != htmlGridEX)
			htmlGridEX.setActive(); 
			
		if(getGridEXRow() != null)
			getGridEXRow().ShowHeaderIndicator(); 
			
		resetRootTableScroll(getRootTable());
		if(resizeHeight || resizeWidth)
		{	
			resizeMode = -1;
			if(resizeHeight)
				getRootTable().ResizeHeight(); 
			
			if(resizeWidth)
				AutoSizeColumns(); 		
		}		
		window.event.returnValue = true;
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
		{
			if(htmlGridEX.contains(window.event.srcElement)) 
			{	
				if(window.event.srcElement.tagName == "SELECT")
					return false;
					
				return true; 
			}			
		}				
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
			return false; 
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
		else if(window.event.keyCode == 113)
		{
			if(isValidEventForRow() && getGridEXRow().getCurrentCell() != null)
			{
				if(getGridEXRow().getRowType() == "Record" || getGridEXRow().getRowType() == "NewRecord")
					getGridEXRow().getCurrentCell().ShowEditorCell();
				else if(getGridEXRow().getRowType() == "FilterRow")
					getGridEXRow().getCurrentCell().ShowFilterCell(); 
			}			
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
	function Initialize()
	{		
		if(groupByBox != null || rootTable != null)
			throw Error("initialize operation could be performed only once"); 
	
		groupByBox  = setGroupByBox(gridEX);		
		rootTable = setRootTable(gridEX);
		if(isdropdown)
		{
			if(selectedItemsCollection == null)
			{
				selectedItemsCollection = new GridEXSelectedItemCollection(gridEX, selectedItems); 
				delete selectedItems;
				selectedItems = null;
			}
		}
	}	
	function ReportRowsStatus()
	{
		var rowstatus = ""; 
		var input = document.getElementById(getID() + "_rowstatus"); 
		if(input != null && gridEXRows != null)
		{					
			var r = null;
			var l = gridEXRows.length; 
			for(var i = 0; i < l; i++)
			{
				r = gridEXRows[i]; 
				if(r.getType() == 3 || r.getType() == 8 || r.getType() == 9 || r.getType() == 11)
				{					
					if(rowstatus.length > 0)
						rowstatus += "|"; 
					rowstatus += r.ReportStatus(); 
				}				
			}
			if(rowstatus.length > 0)
				input.value = rowstatus; 				
		}
	}	
	function gridEX_onmousewheel()
	{
		if(document.activeElement != htmlGridEX)
			htmlGridEX.setActive(); 		
	}	
	function gridEX_onsubmit()
	{				
		if(getUpdateOnLeave())
			UpdateData(false); 			
		ReportRowsStatus();
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
		if(getHtmlGridEX().getAttribute("rtl") == "1")
		{
			if(getRootTable().getHtmlItemsTable().offsetParent.scrollLeft >= 0)
				getRootTable().table_onscroll();
		}
		if(resizeHeight)
		{
			if(isVisible())
				getRootTable().ResizeHeight();
		}		
		if(getColumnAutoResize())
		{	
			if(isVisible())
			{					
				if(clientWidth != -1 && clientWidth != getHtmlWidth())
					resizeMode = 1;
					
				AutoSizeColumns();			
				resizeMode = -1; 
			}
		}
		else
		{	
			if(isVisible())
			{
				if(clientWidth != -1 && clientWidth != getHtmlWidth())
					resizeMode = 1;
					
				resizeMode = -1; 
			}
		}
		if(selectedItemsCollection == null)
		{
			selectedItemsCollection = new GridEXSelectedItemCollection(gridEX, selectedItems); 		
			delete selectedItems;
			selectedItems = null;
		}
		var element = document.getElementById(getID() + "_scrollstatus");
		if(element != null && element.value != "-1")
			getRootTable().getHtmlItemsTable().offsetParent.scrollTop = parseInt(element.value, 10); 
		if(initRowID != null && initRowID.length > 0)
		{
			var _initRootRow = document.getElementById(initRowID); 
			if(_initRootRow != null && _initRootRow.style.display != "none")
			{
				var _initRow = RetrieveRow(_initRootRow, getInnerItemRow(_initRootRow), null, null); 
				setCurrentRow(_initRow, (selectedItemsCollection == null || selectedItemsCollection.Count() == 0 || getSelectionMode() == 2) ? false : true);
				if(!_initRow.getVisibleInScroll())
					getRootTable().getHtmlItemsTable().offsetParent.scrollTop = _initRootRow.offsetTop;
	
				if(htmlGridEX.setActive != null)
					htmlGridEX.setActive(); 
			}
		}		
		FireEvent("GridEXLoad", [gridEX]);
		ReportRowsStatus();
		initialized = true;
		return true; 
	}	
	function gridEX_onunload()
	{	
		unloadArray(controlsToBuild); 
		delete controlsToBuild;
		controlsToBuild = null;
		unloadObjectsArray(editControls);
		delete editControls;
		editControls = null;
		unloadRows(gridEXRows); 
		delete gridEXRows;
		gridEXRows = null;
		unloadArray(eventhandlers);
		delete eventhandlers;
		eventhandlers = null; 
		unloadArray(hiddenValuesCollection); 
		delete hiddenValuesCollection;
		hiddenValuesCollection = null;
		unloadArray(arrRowsCss); 
		delete arrRowsCss;
		arrRowsCss = null;
		unloadArray(selectedItems); 
		unloadArray(tableDefinition); 
		delete tableDefinition;
		tableDefinition = null;
		if(tables != null)
			tables.Unload();
		delete tables;
		tables = null; 
		if(rootTable != null)
			rootTable.Unload();					
		delete rootTable;
		rootTable = null;
		if(gridEXRow != null)
			gridEXRow.Unload(); 			
		if(groupByBox != null)
			groupByBox.Unload();
		delete groupByBox;
		groupByBox = null;
		if(selectedItemsCollection != null)
			selectedItemsCollection.Unload(); 
		delete selectedItemsCollection;
		selectedItemsCollection = null;
		gridEXRow = null;
		tableDefinition = null;
		hiddenValuesCollection = null;
		eventhandlers = null;		
		gridEXRow = null;
		if(htmlGridEX != null)
		{
			htmlGridEX.detachEvent("onmousemove", gridEX_onmousemove);
			htmlGridEX.detachEvent("onmouseup", gridEX_onmouseup);  
			htmlGridEX.detachEvent("onselectstart", gridEX_onselectstart);	
		}
		htmlGridEX = null;		
		htmlGridEXParent = null;
		gridEX = null;		
	}		
	function body_onselectstart()
	{
		if(htmlGridEX.contains(document.activeElement))		
		{
			var element = document.activeElement;
			if(element.tagName != null)
			{
				if(element.tagName == "INPUT" && element.type != null && (element.type == "text" || element.type == "password"))
					return;
				if(element.tagName == "TEXTAREA")
					return;				
			}
			window.event.cancelBubble = true;
			window.event.returnValue = false;
			return false; 
		}
	}		
	function getHtmlGridEX() { return htmlGridEX; }	
	function getHtmlWidth()
	{													
		var width = -1; 
		if(htmlGridEX.style.width != "" && htmlGridEX.style.width.indexOf("%") > 0)
			width = htmlGridEXParent.clientWidth * (getPercentWidth(htmlGridEX.style.width) / 100); 
		else if(htmlGridEX.style.pixelWidth != 0)
			width = htmlGridEX.style.pixelWidth; 
		else
			width = htmlGridEX.offsetWidth;
			
		if(htmlGridEXParent.tagName == "BODY" && htmlGridEX.style.width.indexOf("%") > 0)  
			width -=  (getPixelWidth(htmlGridEXParent.currentStyle.marginLeft) + getPixelWidth(htmlGridEXParent.currentStyle.marginRight)); 
		if(getRootTable().getHtmlItemsTable().offsetParent.scrollHeight >= getRootTable().getHtmlItemsTable().offsetParent.offsetHeight)
		{			
			if(getRootTable().getHtmlItemsTable().offsetHeight != getRootTable().getHtmlItemsTable().offsetParent.offsetHeight)
			{
				if(getRootTable().getHtmlItemsTable().getAttribute("empty") == null)
					width -= 17; 
			}
		}
		var offset = 0; 
		offset += getPixelWidth(htmlGridEX.currentStyle.borderLeftWidth); 
		offset += getPixelWidth(htmlGridEX.currentStyle.borderRightWidth); 		
		return width - offset; 
	}	
	function getResizeWidth()
	{
		if(resizeMode == -1)
			return getHtmlWidth(); 
		else
		{
			var width = clientWidth; 
			if(getRootTable().getHtmlItemsTable().offsetParent.scrollHeight >= getRootTable().getHtmlItemsTable().offsetParent.offsetHeight)
				width -= 17;	 
			return width; 
		}
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
		var divs = gridex.getHtmlGridEX().all.tags("DIV"); 
		var l = divs.length; 
		if(divs != null && l > 0)
		{
			var div = null; 
			for(var i=0; i<l; i++)
			{
				div = divs[i]; 
				if(div.type != null && parseInt(div.type, 10) == 5)
					return new GridEXGroupByBox(div, gridex); 
			}
		}
		return null; 
	}	
	function setRootTable(gridex)
	{				
		var divs = gridex.getHtmlGridEX().all.tags("DIV"); 
		var l = divs.length; 
		if(divs != null && l > 0)
		{
			var _div = null; 
			for(var i=0; i<l; i++)
			{				
				_div = divs[i]; 
				if(_div.type != null && parseInt(_div.type, 10) == 3)
					return new GridEXTable(_div, _div.id, null, 0, gridex);
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
			
		var _t = null; 
		for(var i=0; i<childTables.Count(); i++)
		{
			_t = childTables.getTableInIndex(i); 
			tables.Add(_t);
			copyChildTables(tables, _t); 
		}		
	}	
	var gridEX = this;
	window.attachEvent("onkeydown", function() { ggridEX_onkeydown(id); });	
	if(!isDropDown())
	{	
		if(document.getElementById(formID) != null)
			document.getElementById(formID).attachEvent("onsubmit", function() { ggridEX_onsubmit(id); });		
		
		window.attachEvent("onresize", function() { ggridEX_onresize(id); } );			
	}		
	document.body.attachEvent("onmousemove", body_onmousemove);
	document.body.attachEvent("onselectstart", function() { gbody_onselectstart(id); }); 
	htmlGridEX.attachEvent("onblur", function() { ggridEX_onblur(id); }); 
	htmlGridEX.attachEvent("onkeydown", function() { ggridEX_onkeydown(id); } ); 
	htmlGridEX.attachEvent("onmousemove", gridEX_onmousemove);
	htmlGridEX.attachEvent("onmouseup", gridEX_onmouseup);  
	htmlGridEX.attachEvent("onselectstart", gridEX_onselectstart);		
	htmlGridEX.attachEvent("onmousewheel", function() { ggridEX_onmousewheel(id); } );	
	if(!isDropDown())
		window.attachEvent("onload", function() { ggridEX_onload(id); });				
	window.attachEvent("onunload", function() { ggridEX_onunload(id); });	
	return this; 
}