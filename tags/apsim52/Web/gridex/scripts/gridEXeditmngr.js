//////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////////////
function GridEXCheckBoxManager(cell)
{
	var checkbox = null; 	
	var gridEXCell = cell;	
	if(gridEXCell.getInnerCell().childNodes[0].childNodes.length == 1)
	{				
		var element = gridEXCell.getInnerCell().childNodes[0].childNodes[0]; 
		if(element.nodeType == 1 && element.tagName == "INPUT" && element.type == "checkbox")
			checkbox = element;
	}		
	this.Show = Show;		
	function Show() { }			
	return this;
}
function GridEXCalendarComboDropDownManager(cell)
{
	var gridEXCell = cell; 
	var calendar = gridEXCell.getGridEX().getEditControl(4); 	
	var originalDisplay = null;
	var originalValue = null; 	
	this.getCell = getCell; 
	this.KeyDown = KeyDown; 
	this.KeyUp = KeyUp; 
	this.ParseEditValue = ParseEditValue; 
	this.ValueChanged = ValueChanged; 		
	this.Show = Show;	 		
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue(); 
		else
			originalValue = gridEXCell.getValue();			
		var e = gridEXCell.getInnerSpan(); 		
		if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
				originalDisplay = e.childNodes[0].data; 
		}
	}	
	function updateInnerCell(display)
	{
		var e = gridEXCell.getInnerSpan(); 		
		if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
			{
				if(display == null || display.length == 0)
					e.innerHTML = "&nbsp;";
				else				
					e.childNodes[0].data = display; 
			}
		}
		else if(e.childNodes.length == 0)
		{
			if(display == null)
				e.appendChild(document.createTextNode('')); 
			else
				e.appendChild(document.createTextNode(' ' + display));
		}
	}
	function ParseEditValue(d,t)
	{		
		var args = new ParseEditValueArgs(gridEXCell,d,t);
		gridEXCell.getGridEX().FireEvent("ParseEditValue",[args]);
		d = args.getValue();
		return d;
	}		
	function ValueChanged()
	{
		var dateAsText = calendar.getSelectedDateString();		
		var args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dateAsText); 
		var cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);  
		if(cancel == null || !cancel)
		{				
			if(args != null)		
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(dateAsText);
			gridEXCell.setText(dateAsText);
			gridEXCell.getRow().ShowHeaderIndicator(true); 						
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ArrowDownKeyDown()
	{
		calendar.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowLeftKeyDown()
	{
		if(window.event.srcElement == calendar.getInnerTextBox() || window.event.srcElement.contains(calendar.getInnerTextBox()))
			return; 		
		calendar.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();				
		gridEXCell.getRow().PreviousFocusCell();		
	}
	function ArrowRightKeyDown()
	{
		if(window.event.srcElement == calendar.getInnerTextBox() || window.event.srcElement.contains(calendar.getInnerTextBox()))
			return; 		
		calendar.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();				
		gridEXCell.getRow().NextFocusCell(); 		
	}
	function ArrowUpKeyDown()
	{
		calendar.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function EnterKeyDown()
	{		
		calendar.Hide();
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{			
		gridEXCell.UndoChanges();
		calendar.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
	}
	function TabKeyDown()	
	{	
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 
	}
	function KeyDown()
	{
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false; 
		}
		if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown();
		else if(window.event.keyCode == 9)
			TabKeyDown();
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown();
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown();
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown();			
		if(calendar.getSelectedDateString() != originalDisplay)
			gridEXCell.getRow().ShowHeaderIndicator(true); 
	}
	function KeyUp() { gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); }
	function getCell() { return gridEXCell; }
	function Show()
	{	
		var args = null; 
		var cancel = null; 				
		if(gridEXCell.getRow().getRowType() == "NewRecord")
		{				
			if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
				args = new GridEXEditingArgs(gridEXCell, gridEXCell.getInnerCell().getAttribute("default"));
			else
				args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue());
		}
		else
			args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue());  
		cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 			
		if(cancel == null || !cancel)
		{	
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell(); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell);			
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}			
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getPaddingTop(gridEXCell.getInnerSpan()) + getBorderTopWidth(_innerCell);
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = getRealWidth(_innerCell,false); 
			var _height = getRealHeight(_innerCell); 
			calendar.setLeft(_left); 
			calendar.setTop(_top); 
			calendar.setWidth(_width); 
			calendar.setHeight(_height);			
			calendar.setStyle(gridEXCell.getInnerSpan().currentStyle); 			
			calendar.setOwner(editManager);			
			keepOriginalValues(args); 			
			var _input = calendar.getInnerTextBox(); 
			_input.style.padding = gridEXCell.getInnerSpan().currentStyle.padding; 			
			if(args != null)
				calendar.setSelectedDate(args.getValue()); 
			else
			{
				if(gridEXCell.getRow().getRowType() == "NewRecord")
				{
					if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
						calendar.setSelectedDate(gridEXCell.getInnerCell().getAttribute("default"));
					else if(gridEXCell.getDataChanged())
						calendar.setSelectedDate(gridEXCell.getValue()); 
					else if(gridEXCell.getInnerCell().getAttribute("ind") != null && gridEXCell.getInnerCell().getAttribute("value") != null)
						calendar.setSelectedDate(gridEXCell.getInnerCell().getAttribute("value")); 
				}
				else
					calendar.setSelectedDate(gridEXCell.getValue()); 
			}
			calendar.Show(); 
		}		
	}
	var editManager = this; 	
	return this; 
}
function GridEXCalendarDropDownManager(cell)
{
	var gridEXCell = cell;
	var calendar = gridEXCell.getGridEX().getEditControl(3); 	
	var originalDisplay = null; 
	var originalValue = null;
	this.getCell = getCell; 
	this.KeyDown = KeyDown; 
	this.ParseEditValue = ParseEditValue; 
	this.ValueChanged = ValueChanged; 	
	this.Show = Show; 	
	function ValueChanged()
	{		
		var dateAsText = calendar.getSelectedDateString(); 				
		var args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dateAsText); 
		var c = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]); 				
		if(c == null || !c)
		{	
			if(args != null)		
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(dateAsText);				
			if(args != null)
				gridEXCell.setText(args.getValue()); 
			else
				gridEXCell.setText(dateAsText);
			gridEXCell.getRow().ShowHeaderIndicator(true); 						
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]); 			
		}
		if(c != null && c == true)
			return true; 
	}
	function ParseEditValue(d,t)
	{		
		var args = new ParseEditValueArgs(gridEXCell,d,t);
		gridEXCell.getGridEX().FireEvent("ParseEditValue",[args]);
		d = args.getValue();
		return d;
	}	
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue(); 
		else
			originalValue = gridEXCell.getValue();			
		var e = gridEXCell.getInnerSpan();		
		if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
				originalDisplay = e.childNodes[0].data; 
		}
	}	
	function updateInnerCell(display)
	{
		var e = gridEXCell.getInnerSpan(); 		
		if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
			{
				if(display == null || display.length == 0)
					e.innerHTML = "&nbsp;"; 
				else				
					e.childNodes[0].data = display;
			}
		}
		else if(e.childNodes.length == 0)
			e.appendChild(document.createTextNode(' ' + display));
	}	
	function ArrowDownKeyDown()
	{
		calendar.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowLeftKeyDown()
	{		
		calendar.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();			
		gridEXCell.getRow().PreviousFocusCell(); 
	}
	function ArrowRightKeyDown()
	{
		calendar.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();			
		gridEXCell.getRow().NextFocusCell(); 
	}
	function ArrowUpKeyDown()
	{
		calendar.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MovePrevious(); 
	}	
	function EscKeyDown()
	{		
		gridEXCell.UndoChanges(); 
		calendar.Hide(); 		
	}	
	function EnterKeyDown() { gridEXCell.ResumeEdit(); }
	function TabKeyDown()	
	{	
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 		
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 
	}	
	function KeyDown()
	{
		var c = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(c != null && c == true)
		{
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
			return false;
		}
		if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown();
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown();		
	}
	function getCell() { return gridEXCell; }
	function Show()
	{	
		var args = null; 
		var cancel = null; 				
		if(gridEXCell.getRow().getRowType() == "NewRecord")
		{				
			if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
				args = new GridEXEditingArgs(gridEXCell, gridEXCell.getInnerCell().getAttribute("default"));
			else
				args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue());
		}
		else
			args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
		cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 
		if(cancel == null || !cancel)
		{	
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1"); 
			var _innerCell = gridEXCell.getInnerCell(); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell);
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}			
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getPaddingTop(gridEXCell.getInnerSpan()) + getBorderTopWidth(_innerCell);
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 
			var _width =  getRealWidth(_innerCell,false);
			var _height = getRealHeight(_innerCell,false); 
			calendar.setLeft(_left); 
			calendar.setTop(_top); 
			calendar.setWidth(_width); 
			calendar.setHeight(_height);			
			calendar.setOwner(editManager);			
			keepOriginalValues(args); 			
			if(args != null)			
				calendar.setSelectedDate(args.getValue()); 
			else
			{
				if(gridEXCell.getRow().getRowType() == "NewRecord")
				{
					if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
						calendar.setSelectedDate(gridEXCell.getInnerCell().getAttribute("default"));
					else if(gridEXCell.getDataChanged())
						calendar.setSelectedDate(gridEXCell.getValue()); 
					else if(gridEXCell.getInnerCell().getAttribute("ind") != null && gridEXCell.getInnerCell().getAttribute("value") != null)
						calendar.setSelectedDate(gridEXCell.getInnerCell().getAttribute("value")); 
				}
				else
					calendar.setSelectedDate(gridEXCell.getValue());
			}
			calendar.Show(); 			
		}
	}		
	var editManager = this;
	return this; 
}
function GridEXComboDropDownManager(cell)
{
	var gridEXCell = cell;
	var dropdown = gridEXCell.getGridEX().getEditControl(7, gridEXCell.getColumn().getDropDownID()); 	
	var originalValue = null;
	var originalDisplay = null; 
	var originalImage = null; 	
	this.getCell = getCell; 	
	this.DropDown = DropDown;
	this.KeyDown = KeyDown;
	this.KeyUp = KeyUp; 
	this.NotInList = NotInList;
	this.ValueChanged = ValueChanged; 	
	this.Show = Show; 	
	function getCell() { return gridEXCell; }	
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue(); 
		else
			originalValue = gridEXCell.getValue();					
		var e = gridEXCell.getInnerSpan();
		if(e.childNodes.length == 0)
			return;						
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG")
				originalImage = e.childNodes[0].src; 				
			if(e.childNodes[1].nodeType == 3)
			{
				originalDisplay = e.childNodes[1].data; 
				var xdisplay = dropdown.getDisplayByValue(originalValue);
				if(xdisplay != null && xdisplay != originalDisplay)
					originalDisplay = xdisplay; 
			}
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
			{
				originalDisplay = e.childNodes[0].data; 
				var xdisplay = dropdown.getDisplayByValue(originalValue);
				if(xdisplay != null && xdisplay != originalDisplay)
					originalDisplay = xdisplay; 
			}
		}		
	}	
	function updateInnerCell(display, image)
	{	
		var e = gridEXCell.getInnerSpan(); 		
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG" && image != null)
			{
				_imgToUpdate = e.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(e.childNodes[1].nodeType == 3)
			{
				if(display == null)
					e.childNodes[1].data = "";
				else
					e.childNodes[1].data = display;
			}
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
			{
				if(display == null)
					e.childNodes[0].data = ""; 
				else
					e.childNodes[0].data = display; 
			}
		}
		else if(e.childNodes.length == 0)
		{
			if(image != null)
			{				
				var _img = document.createElement("IMG"); 
				_img.align = "absmiddle"; 
				_img.border = "0"; 
				_img.height = "15px"; 
				_img.src = image; 
				e.appendChild(_img); 
			}
			if(display == null)
				e.appendChild(document.createTextNode(''));
			else
				e.appendChild(document.createTextNode(' ' + display));
		}
	}
	function Show()
	{		
		var args = null; 
		var cancel = null;		
		if(gridEXCell.getRow().getRowType() == "NewRecord")
		{
			if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
				args = new GridEXEditingArgs(gridEXCell, gridEXCell.getInnerCell().getAttribute("default")); 
			else
				args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
		}
		else
			args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue());
		cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]);
		if(cancel == null || !cancel)
		{	
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell(); 
			var _innerSpan = gridEXCell.getInnerSpan(); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell);
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}			
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getPaddingTop(gridEXCell.getInnerSpan()) + getBorderTopWidth(_innerCell);
			if(!gridEXCell.isNewRecordTopCell())			
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 			
			var _width = getRealWidth(_innerCell,false); 
			var _height = getRealHeight(_innerCell); 
			dropdown.setLeft(_left); 
			dropdown.setTop(_top); 
			dropdown.setWidth(_width); 
			dropdown.setHeight(_height); 
			dropdown.setStyle(_innerCell.currentStyle);
			dropdown.setOwner(editManager);
			dropdown.setCharacterCasing(gridEXCell.getColumn().characterCasing); 
			var _input = dropdown.getInnerTextBox(); 
			_input.style.padding = _innerSpan.currentStyle.padding;
			if(args != null)
			{
				if(gridEXCell.getInnerCell().getAttribute("niv") == null)
					dropdown.setValue(args.getValue()); 
				else
					dropdown.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
			}
			else
			{
				if(gridEXCell.getRow().getRowType() == "NewRecord")
				{
					if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
						dropdown.setValue(gridEXCell.getInnerCell().getAttribute("default"));
					else if(gridEXCell.getDataChanged())
						dropdown.setValue(gridEXCell.getValue()); 
					else if(gridEXCell.getInnerCell().getAttribute("niv") != null)
						dropdown.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
					else if(gridEXCell.getInnerCell().getAttribute("ind") != null && gridEXCell.getInnerCell().getAttribute("value") != null)
						dropdown.setValue(gridEXCell.getValue()); 
				}
				else
				{
					if(gridEXCell.getInnerCell().getAttribute("niv") == null)
						dropdown.setValue(gridEXCell.getValue());
					else
						dropdown.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
				}
			}
			keepOriginalValues(args);
			dropdown.Show();
		}
	}	
	function DropDown()
	{		
		if(gridEXCell.getGridEX().ddpb)
		{
			var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
			if(cancel == null || !cancel)
			{
				var _gdd = eval(dropdown.getID());
				if(_gdd == null)
					throw new Error("invalid gridex dropdown");				
				if(_gdd.getHtmlGridEX().getAttribute("fdd") != "1")
				{
					if(_gdd.getHtmlGridEX().getAttribute("lar") == gridEXCell.getRow().getID())
						return; 
				}							
				var input = document.getElementById(gridEXCell.getGridEX().getID() + "_eventdata"); 
				input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();
				gridEXCell.getGridEX().DoPostBack(null, "DropDown", true); 
				return false;
			}
		}		
	}	
	function KeyDown()
	{
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false; 
		}
		if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown();
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown();
		if(originalDisplay != dropdown.getInnerTextBox().value)
			gridEXCell.getRow().ShowHeaderIndicator(true); 
	}
	function KeyUp() { gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); }	
	function ArrowDownKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowLeftKeyDown()
	{
		if(window.event.srcElement == dropdown.getInnerTextBox() || window.event.srcElement.contains(dropdown.getInnerTextBox()))
			return; 			
		dropdown.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getRow().PreviousFocusCell(); 
	}
	function ArrowRightKeyDown()
	{
		if(window.event.srcElement == dropdown.getInnerTextBox() || window.event.srcElement.contains(dropdown.getInnerTextBox()))
			return; 			
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getRow().NextFocusCell(); 
	}
	function ArrowUpKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function EnterKeyDown()
	{
		dropdown.Hide(); 
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		if(gridEXCell.originalValue != dropdown.getValue())		
			gridEXCell.UndoChanges(); 					
		dropdown.Hide();
		gridEXCell.getInnerCell().setAttribute("niv", null); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();						
	}
	function TabKeyDown()
	{
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 		
	}
	function ValueChanged(value)
	{		
		var args = null; 
		var cancel = null; 
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dropdown.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);  
		}			
		if(cancel == null || !cancel)
		{
			if(value == null)
			{
				gridEXCell.setText(dropdown.getDisplay(), originalImage);
				if(args != null)		
					gridEXCell.setValue(args.getValue()); 
				else
					gridEXCell.setValue(dropdown.getValue());
			}
			else
				gridEXCell.setValue(originalValue); 
				
			gridEXCell.getInnerCell().setAttribute("niv", null); 			
			gridEXCell.getRow().ShowHeaderIndicator(true); 						
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]); 
		}
	}
	function NotInList(value)
	{
		if(gridEXCell.getColumn().limitToList)
		{
			gridEXCell.getInnerCell().setAttribute("niv", value); 
			gridEXCell.dataChanged = true; 
			updateInnerCell(value, originalImage);
		}
	}	
	var editManager = this; 	
	return this; 	
}
function GridEXDropDownManager(cell)
{
	var gridEXCell = cell;
	var dropdown = gridEXCell.getGridEX().getEditControl(8, gridEXCell.getColumn().getDropDownID()); 	
	var originalValue = null; 
	var originalDisplay = null; 
	var originalImage = null;	
	this.getCell = getCell; 	
	this.DropDown = DropDown; 
	this.KeyDown = KeyDown; 	
	this.ValueChanged = ValueChanged;	
	this.Show = Show; 	
	function getCell() { return gridEXCell; }		
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue();
		else
			originalValue = gridEXCell.getValue();		
		var e = gridEXCell.getInnerSpan(); 
		if(e.childNodes.length == 0)
			return;			
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG")
				originalImage = e.childNodes[0].src; 				
			if(e.childNodes[1].nodeType == 3)
				originalDisplay = e.childNodes[1].data; 
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
				originalDisplay = e.childNodes[0].data; 
		}		
	}	
	function updateInnerCell(display, image)
	{	
		var e = gridEXCell.getInnerSpan(); 				
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG" && image != null)
			{
				_imgToUpdate = e.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(e.childNodes[1].nodeType == 3)
				e.childNodes[1].data = display;
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
				e.childNodes[0].data = display; 
		}
		else if(e.childNodes.length == 0)
		{
			if(image != null)
			{				
				var _img = document.createElement("IMG"); 
				_img.align = "absmiddle"; 
				_img.border = "0"; 
				_img.height = "15px"; 
				_img.src = image; 
				e.appendChild(_img); 
			}						
			e.appendChild(document.createTextNode(' ' + display));
		}
	}		
	function DropDown()
	{		
		if(gridEXCell.getGridEX().ddpb)
		{
			var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
			if(cancel == null || !cancel)
			{
				var _gdd = eval(dropdown.getID());
				if(_gdd == null)
					throw new Error("invalid gridex dropdown");				
				if(_gdd.getHtmlGridEX().getAttribute("fdd") != "1")
				{
					if(_gdd.getHtmlGridEX().getAttribute("lar") == gridEXCell.getRow().getID())
						return; 
				}								
				var input = document.getElementById(gridEXCell.getGridEX().getID() + "_eventdata"); 
				input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();
				gridEXCell.getGridEX().DoPostBack(null, "DropDown", true); 
				return false; 		
			}
		}
	}
	function EnterKeyDown() { gridEXCell.ResumeEdit(); }	
	function EscKeyDown()
	{
		if(gridEXCell.originalValue != dropdown.getValue())		
			gridEXCell.UndoChanges(); 			
		dropdown.Hide();		
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();		
	}
	function TabKeyDown()
	{
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true); 		
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 		
	}
	function ArrowRightKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getRow().NextFocusCell(); 
	}
	function ArrowLeftKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();			
		gridEXCell.getRow().PreviousFocusCell(); 
	}
	function KeyDown()
	{
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
			return false;
		}
		if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown();
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
	}
	function ArrowDownKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MoveNext(); 
	}	
	function ArrowUpKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function ValueChanged()
	{	
		var args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dropdown.getValue()); 
		var cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);  				
		if(cancel == null || !cancel)
		{			
			if(args != null)
				gridEXCell.setValue(args.getValue()); 
			else		
				gridEXCell.setValue(dropdown.getValue());
			gridEXCell.setText(dropdown.getDisplay(), originalImage);
			gridEXCell.getRow().ShowHeaderIndicator(true); 			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
				gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]); 
		}
	}	
	function Show()
	{
		var args = null; 
		var cancel = null; 		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			if(gridEXCell.getRow().getRowType() == "NewRecord")
			{
				if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
					args = new GridEXEditingArgs(gridEXCell, gridEXCell.getInnerCell().getAttribute("default"));
				else
					args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
			}
			else
				args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 
		}		
		if(cancel == null || !cancel)
		{	
			var _isrtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell(); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell); 
			if(_isrtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}
			if(!gridEXCell.isNewRecordTopCell() && !_isrtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			else if(gridEXCell.isNewRecordTopCell() && _isrtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getPaddingTop(gridEXCell.getInnerSpan()) + getBorderTopWidth(_innerCell);
			if(!gridEXCell.isNewRecordTopCell())			
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 
			var _width = getRealWidth(_innerCell,false); 
			var _height =  getRealHeight(_innerCell,false); 
			dropdown.setLeft(_left); 
			dropdown.setTop(_top); 
			dropdown.setWidth(_width); 
			dropdown.setHeight(_height); 			
			dropdown.setOwner(editManager);			
			if(args != null)
				dropdown.setValue(args.getValue()); 
			else
			{
				if(gridEXCell.getRow().getRowType() == "NewRecord")
				{
					if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
						dropdown.setValue(gridEXCell.getInnerCell().getAttribute("default"));
					else if(gridEXCell.getDataChanged())
						dropdown.setValue(gridEXCell.getValue()); 
					else if(gridEXCell.getInnerCell().getAttribute("ind") != null && gridEXCell.getInnerCell().getAttribute("value") != null)
						dropdown.setValue(gridEXCell.getInnerCell().getAttribute("value")); 
				}
				else
					dropdown.setValue(gridEXCell.getValue());
			}
			keepOriginalValues(args); 			
			dropdown.Show(); 
		}
	}	
	var editManager = this;
	return this;
}
function GridEXEditTextAreaManager(cell)
{
	var gridEXCell = cell; 
	var originalValue = null; 
	var textarea = gridEXCell.getGridEX().getEditControl(14); 	
	this.KeyDown = KeyDown;
	this.KeyUp = KeyUp; 
	this.Leaving = Leaving; 
	this.Show = Show; 
	this.TextChanged = TextChanged; 	
	function EscKeyDown()
	{
		textarea.Hide();		
		if(gridEXCell.originalValue != textarea.getText())
			gridEXCell.UndoChanges();						

		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();		
	}	
	function TabKeyDown()
	{		
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true); 

		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 		
	}	
	function KeyDown()
	{
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false; 
		}	
		if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
	}
	function KeyUp() { gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); }
	function Leaving() { textarea.Hide(); }	
	function Show()
	{
		var args = null; 	
		var cancel = null;		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 
		}		
		if(cancel == null || !cancel)
		{		
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell();
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell); 			
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}			
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getBorderTopWidth(_innerCell); 
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = getRealWidth(_innerCell); 
			var _height = getRealHeight(_innerCell);
			textarea.setLeft(_left); 
			textarea.setTop(_top); 
			textarea.setWidth(_width); 
			textarea.setHeight(_height);
			textarea.setCharacterCasing(gridEXCell.getColumn().characterCasing); 
			textarea.setStyle(_innerCell.currentStyle); 			
			if(args != null)			
			{
				originalValue = args.getValue(); 
				textarea.setText(args.getValue()); 
			}
			else
			{
				originalValue = gridEXCell.getValue(); 
				textarea.setText(gridEXCell.getValue()); 		
			}			
			var _textarea = textarea.getInnerTextArea(); 
			_textarea.style.padding = _innerCell.childNodes[0].currentStyle.padding; 
			switch(gridEXCell.getColumn().getScrollBars())
			{			
				case 2:
				{
					_textarea.style.overflowX = "auto"; 
					_textarea.style.overflowY = "hidden"; 
				} break; 				
				case 3:
				{
					_textarea.style.overflowY = "auto"; 
					_textarea.style.overflowX = "hidden"; 				
				} break; 				
				case 4:
				{
					_textarea.style.overflowY = "auto"; 
					_textarea.style.overflowX = "auto"; 
				} break; 				
				case 1:
				{
					_textarea.style.overflowX = "hidden"; 
					_textarea.style.overflowY = "hidden"; 
				} break; 
			}			
			textarea.setOwner(editManager);
			textarea.Show();
			textarea.Focus();
		}		
	}	
	function TextChanged()
	{	
		var args = null; 	
		var cancel = null;		
		var newText = textarea.getText(); 
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, newText); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]); 
		}			
		if(cancel == null || !cancel)
		{			
			if(args != null)		
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(newText);							
			gridEXCell.setText(newText);
			gridEXCell.getRow().ShowHeaderIndicator(true); 			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
				gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell] ); 		
		}
	}
	var editManager = this;	
	return editManager;
}
function GridEXEditPasswordManager(cell)
{
	var gridEXCell = cell;
	var originalValue = null; 
	var textbox = gridEXCell.getGridEX().getEditControl(9); 	
	this.ApplyingInputMask = ApplyingInputMask;
	this.Leaving = Leaving; 
	this.KeyDown = KeyDown; 
	this.KeyUp = KeyUp; 
	this.MouseWheel = MouseWheel; 
	this.TextChanged = TextChanged;	
	this.Show = Show; 	
	this.Hide = Hide; 
	function Show()
	{	
		var args = null;
		var cancel = null;		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 
		}		
		if(cancel == null || !cancel)
		{	
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell();			
			var _left = -1;
			if(gridEXCell.getRow().getTable().getHierarchicalMode() != 2)
				_left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell);
			else
				_left = getPixelLeft(gridEXCell.getInnerSpan());
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}			
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);				
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCell.childNodes[0]) + getBorderTopWidth(_innerCell);
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = getRealWidth(_innerCell); 
			var _height = getRealHeight(_innerCell); 
			textbox.setLeft(_left); 
			textbox.setTop(_top); 
			textbox.setWidth(_width); 
			textbox.setHeight(_height); 
			textbox.setStyle(_innerCell.currentStyle); 			
			if(args != null)			
			{
				originalValue = args.getValue(); 
				textbox.setText(originalValue); 
			}
			else
			{
				originalValue = gridEXCell.getValue(); 
				textbox.setText(originalValue);  
			}			
			var _input = textbox.getInnerHTML(); 
			_input.style.padding = _innerCell.childNodes[0].currentStyle.padding; 		
			textbox.setInputMask(gridEXCell.getColumn().getInputMask()); 			
			textbox.setInvalidValueAction(gridEXCell.getColumn().getInvalidValueAction()); 
			textbox.setInvalidValueMessage(gridEXCell.getColumn().getInvalidValueMessage());
			textbox.setOwner(editManager);
			textbox.setMaxLength(gridEXCell.getColumn().getMaxLength()); 			
			textbox.Show(); 
			textbox.Focus();			
		}
	}	
	function Hide() { textbox.Hide(); }	
	function KeyDown()
	{		
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false; 
			window.event.cancelBubble = true; 
			return false;
		}
		if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 					
	}	
	function KeyUp()
	{
		if(originalValue != textbox.getInnerHTML().value)
			gridEXCell.getRow().ShowHeaderIndicator(true); 
	}	
	function MouseWheel() { Hide(); }
	function ApplyingInputMask() { return gridEXCell().getGridEX().FireEvent("ApplyingInputMask", [gridEXCell]); }
	function ArrowDownKeyDown()
	{
		Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MoveNext(); 
	}	
	function ArrowUpKeyDown()
	{
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MovePrevious(); 
	}	
	function EnterKeyDown()
	{
		Hide();
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		Hide();		
		if(textbox.getText() != gridEXCell.originalValue)
		{		
			gridEXCell.UndoChanges(); 										
			gridEXCell.getInnerCell().setAttribute("text", originalValue);
		}		
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();		
	}	
	function Leaving() { Hide(); }	
	function TabKeyDown()	
	{	
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true); 		
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 
	}	
	function TextChanged()
	{
		var args = null; 
		var cancel = null; 
		var newText = textbox.getText(); 		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, newText); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);
		}			
		if(cancel == null || !cancel)
		{	
			if(args != null)			
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(newText);			
			gridEXCell.setText(newText); 
			gridEXCell.getInnerCell().setAttribute("text", newText); 
			gridEXCell.getRow().ShowHeaderIndicator(true);						
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}	
	var editManager = this; 	
	return this; 
}
function GridEXEditTextBoxManager(cell)
{
	var gridEXCell = cell;
	var originalValue = null; 
	var textbox = gridEXCell.getGridEX().getEditControl(2);
	this.ApplyingInputMask = ApplyingInputMask; 	
	this.Leaving = Leaving; 
	this.KeyDown = KeyDown; 
	this.KeyUp = KeyUp; 
	this.MouseWheel = MouseWheel; 
	this.TextChanged = TextChanged;	
	this.Show = Show; 	
	this.Hide = Hide; 
	function Show()
	{	
		var args = null;
		var cancel = null;		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 
		}		
		if(cancel == null || !cancel)
		{	
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell();	
			var _left = -1; 
			if(gridEXCell.getRow().getTable().getHierarchicalMode() != 2)
				_left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell);
			else		
				_left = getPixelLeft(gridEXCell.getInnerSpan());			
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getPaddingTop(gridEXCell.getInnerSpan()) + getBorderTopWidth(_innerCell);						
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = getRealWidth(_innerCell); 
			var _height = getRealHeight(_innerCell); 
			textbox.setLeft(_left); 
			textbox.setTop(_top); 
			textbox.setWidth(_width); 
			textbox.setHeight(_height); 
			textbox.setStyle(_innerCell.currentStyle); 			
			if(args != null)			
			{
				originalValue = args.getValue(); 
				textbox.setText(args.getValue()); 
			}
			else
			{
				originalValue = gridEXCell.getValue(); 
				textbox.setText(gridEXCell.getValue());  
			}			
			var _input = textbox.getInnerHTML();			
			_input.style.padding = gridEXCell.getInnerSpan().currentStyle.padding; 		
			textbox.setInputMask(gridEXCell.getColumn().getInputMask()); 			
			textbox.setInvalidValueAction(gridEXCell.getColumn().getInvalidValueAction()); 
			textbox.setInvalidValueMessage(gridEXCell.getColumn().getInvalidValueMessage());
			textbox.setOwner(editManager);
			textbox.setMaxLength(gridEXCell.getColumn().getMaxLength());
			textbox.setCharacterCasing(gridEXCell.getColumn().characterCasing); 
			textbox.Show(); 
			textbox.Focus();			
		}
	}	
	function Hide() { textbox.Hide(); }	
	function KeyDown()
	{		
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false; 
			window.event.cancelBubble = true; 
			return false;
		}
		if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 					
	}	
	function KeyUp()
	{
		if(originalValue != textbox.getInnerHTML().value)
			gridEXCell.getRow().ShowHeaderIndicator(true); 			
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
	}	
	function MouseWheel() { Hide(); }
	function ApplyingInputMask() { return gridEXCell.getGridEX().FireEvent("ApplyingInputMask", [gridEXCell]); }
	function ArrowDownKeyDown()
	{
		Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
		gridEXCell.getGridEX().MoveNext();
		return false; 	
	}	
	function ArrowUpKeyDown()
	{
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MovePrevious(); 		
	}		
	function EnterKeyDown()
	{
		Hide();
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		Hide();				
		gridEXCell.UndoChanges(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();		
	}	
	function Leaving() { Hide(); }	
	function TabKeyDown()	
	{	
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true); 		
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 
	}	
	function TextChanged()
	{
		var newText = textbox.getText(); 			
		var args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, newText); 
		var cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);					
		if(cancel == null || !cancel)
		{	
			if(args != null)			
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(newText);
			
			gridEXCell.setText(newText);
			gridEXCell.getRow().ShowHeaderIndicator(true);						
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}	
	var editManager = this; 	
	return this; 
}
function GridEXEditValueListManager(cell)
{
	var gridEXCell = cell; 
	var valueList = gridEXCell.getGridEX().getEditControl(6, gridEXCell.getColumn().getClientID() + "_ValueList");	
	var originalValue = null;
	var originalDisplay = null;
	var originalImage = null; 	
	this.getCell = getCell; 	
	this.DropDown = DropDown; 	
	this.KeyDown = KeyDown; 
	this.Leaving = Leaving; 
	this.ValueChanged = ValueChanged; 		
	this.Show = Show; 		
	function getCell() { return gridEXCell; }		
	function DropDown()
	{		
		if(valueList.ddpb)
		{
			var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
			if(cancel == null || !cancel)
			{
				if(valueList.getInnerList().getAttribute("fdd") == "1" || valueList.getInnerList().getAttribute("lar") != gridEXCell.getRow().getID())
				{
					var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
					if(cancel == null || !cancel)
					{
						var input = document.getElementById(gridEXCell.getGridEX().getID() + "_eventdata");
						input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();
						gridEXCell.getGridEX().DoPostBack(null, "DropDown", true);
						return false;
					}
				}
			}
		}		
		if(gridEXCell.getRow().getRowType() == "NewRecord")
		{
			if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
				valueList.setValue(gridEXCell.getInnerCell().getAttribute("default")); 
			else if(gridEXCell.getDataChanged())
				valueList.setValue(gridEXCell.getValue());
			else if(gridEXCell.getInnerCell().getAttribute("ind") != null && gridEXCell.getInnerCell().getAttribute("value") != null)
				valueList.setValue(gridEXCell.getInnerCell().getAttribute("value")); 
		}
		else
			valueList.setValue(gridEXCell.getValue());
	}	
	function EnterKeyDown() { gridEXCell.ResumeEdit(); }	
	function EscKeyDown()
	{
		if(gridEXCell.originalValue != valueList.getValue())
			gridEXCell.UndoChanges(); 			
		valueList.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();		
	}	
	function TabKeyDown()
	{
		if(gridEXCell.getGridEX().TabElementChanging != null)		
			gridEXCell.getGridEX().TabElementChanging(true);
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false; 		
	}
	function ArrowDownKeyDown()
	{
		valueList.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();			
		cancelEvent(); 
		gridEXCell.getGridEX().MoveNext(); 
	}	
	function ArrowUpKeyDown()
	{
		valueList.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 					
		cancelEvent(); 
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function ArrowLeftKeyDown()
	{
		valueList.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();			
		gridEXCell.getRow().PreviousFocusCell(); 
	}
	function ArrowRightKeyDown()
	{
		valueList.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();			
		gridEXCell.getRow().NextFocusCell();
	}
	function KeyDown()
	{
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
			return false;
		}
		if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown();
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown();
	}
	function Leaving()
	{
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
	}	
	function ValueChanged()
	{	
		var args = null; 
		var cancel = null;
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, valueList.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]); 
		}			
		if(cancel == null || !cancel)
		{
			if(args != null)			
				gridEXCell.setValue(args.getValue());
			else
				gridEXCell.setValue(valueList.getValue()); 				
			gridEXCell.setText(valueList.getDisplay(),valueList.getImage());
			gridEXCell.getRow().ShowHeaderIndicator(true); 			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
				gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]); 
		}
	}	
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue();
		else
			originalValue = gridEXCell.getValue();		
		var e = gridEXCell.getInnerCell().getElementsByTagName("SPAN")[0]; 
		if(e.childNodes.length == 0)
			return;					
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG")
				originalImage = e.childNodes[0].src; 				
			if(e.childNodes[1].nodeType == 3)
				originalDisplay = e.childNodes[1].data; 
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
				originalDisplay = e.childNodes[0].data; 
		}		
	}	
	function updateInnerCell(display, image)
	{	
		var e = gridEXCell.getInnerCell().getElementsByTagName("SPAN")[0]; 		
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG" && image != null)
			{
				_imgToUpdate = e.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(e.childNodes[1].nodeType == 3)
				e.childNodes[1].data = display;
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
				e.childNodes[0].data = display; 
		}
		else if(e.childNodes.length == 0)
		{
			if(image != null)
			{				
				var _img = document.createElement("IMG"); 
				_img.align = "absmiddle"; 
				_img.border = "0"; 
				_img.height = "15px"; 
				_img.src = image; 
				e.appendChild(_img); 
			}						
			e.appendChild(document.createTextNode(' ' + display));
		}
	}		
	function Show()
	{
		var args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue());
		var cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 
		if(cancel == null || !cancel)
		{	
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell(); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell);
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}			
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getPaddingTop(gridEXCell.getInnerSpan()) + getBorderTopWidth(_innerCell);
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 
			var _width = getRealWidth(_innerCell,false);
			var _height = getRealHeight(_innerCell,false);
			valueList.setOwner(editManager); 			
			valueList.setLeft(_left);
			valueList.setTop(_top); 
			valueList.setWidth(_width); 
			valueList.setHeight(_height);
			if(!gridEXCell.getRow().getTable().getUseColumnSets())
			{
				valueList.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
				valueList.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1)); 						
			}
			else
			{
				valueList.setItemCSS(gridEXCell.getRow().getTable().getRowCss(33));
				valueList.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(34)); 
			}
			keepOriginalValues(args);			
			valueList.Show(); 
			valueList.Focus();			
		}
	}	
	var editManager = this; 		
	return this; 
}
function GridEXComboListManager(cell)
{
	var gridEXCell = cell;	
	var combo = gridEXCell.getGridEX().getEditControl(5, gridEXCell.getColumn().getClientID() + "_Combo"); 	
	var originalValue = null;
	var originalDisplay = null; 
	var originalImage = null; 	
	this.getCell = getCell; 
	this.DropDown = DropDown; 	
	this.KeyDown = KeyDown; 
	this.KeyUp = KeyUp; 
	this.Leaving = Leaving;
	this.NotInList = NotInList; 
	this.ValueChanged = ValueChanged; 	
	this.Show = Show; 	
	function getCell() { return gridEXCell; }	
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue(); 
		else
			originalValue = gridEXCell.getValue();		
		var e = gridEXCell.getInnerSpan(); 
		if(e.childNodes.length == 0)
			return;
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG")
				originalImage = e.childNodes[0].src; 				
			if(e.childNodes[1].nodeType == 3)
			{
				originalDisplay = e.childNodes[1].data;
				var xdisplay = combo.getDisplayByValue(originalValue);
				if(xdisplay != null && xdisplay != originalDisplay)				
					originalDisplay = xdisplay; 
			}
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
			{
				originalDisplay = e.childNodes[0].data;
				var xdisplay = combo.getDisplayByValue(originalValue);
				if(xdisplay != null && xdisplay != originalDisplay)
					originalDisplay = xdisplay; 
			}
		}		
	}	
	function updateInnerCell(display, image)
	{			
		var e = gridEXCell.getInnerSpan(); 		
		if(e.childNodes.length == 2)
		{
			if(e.childNodes[0].nodeType == 1 && e.childNodes[0].tagName == "IMG")
			{
				_imgToUpdate = e.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(e.childNodes[1].nodeType == 3)
				e.childNodes[1].data = display;
		}
		else if(e.childNodes.length == 1)
		{
			if(e.childNodes[0].nodeType == 3)
				e.childNodes[0].data = display; 
		}
		else if(e.childNodes.length == 0)
		{
			if(image != null)
			{				
				var i = document.createElement("IMG"); 
				i.align = "absmiddle"; 
				i.border = "0"; 
				i.height = "15px"; 
				i.src = image; 
				e.appendChild(_img); 
			}						
			e.appendChild(document.createTextNode(' ' + display));
		}
	}		
	function DropDown()
	{			
		if(combo.ddpb)
		{
			var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
			if(cancel == null || !cancel)
			{
				if(combo.getInnerCombo().getAttribute("fdd") == "1" || combo.getInnerCombo().getAttribute("lar") != gridEXCell.getRow().getID())
				{
					var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
					if(cancel == null || !cancel)
					{
						var input = document.getElementById(gridEXCell.getGridEX().getID() + "_eventdata");
						input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();				
						gridEXCell.getGridEX().DoPostBack(null, "DropDown", true);
						return false;
					}
				}
			}			
		}		 
	}	
	function KeyDown()
	{	
		var cancel = gridEXCell.getGridEX().FireEvent("EditingKeyDown", [gridEXCell, window.event.keyCode]); 
		if(cancel != null && cancel == true)
		{
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
			return false;
		}
		if(window.event.keyCode == 9)
			TabKeyDown(); 
		else if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 					
		if(combo.getInnerTextBox().value != originalDisplay)
			gridEXCell.getRow().ShowHeaderIndicator(true); 
	}
	function KeyUp() { gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); }
	function ArrowDownKeyDown()
	{
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowLeftKeyDown()
	{
		if(window.event.srcElement == combo.getInnerTextBox() || window.event.srcElement.contains(combo.getInnerTextBox()))
			return; 			
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getRow().PreviousFocusCell(); 
	}
	function ArrowRightKeyDown()
	{
		if(window.event.srcElement == combo.getInnerTextBox() || window.event.srcElement.contains(combo.getInnerTextBox()))
			return; 			
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getRow().NextFocusCell(); 
	}
	function ArrowUpKeyDown()
	{
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 			
		gridEXCell.getGridEX().MovePrevious(); 
	}	
	function EnterKeyDown() { gridEXCell.ResumeEdit(); }	
	function EscKeyDown()
	{
		if(gridEXCell.originalValue != combo.getValue())
			gridEXCell.UndoChanges(); 			
		gridEXCell.getInnerCell().setAttribute("niv", null); 
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
	}	
	function Leaving() { combo.Hide(); }
	function TabKeyDown()
	{
		if(gridEXCell.getGridEX().TabElementChanging != null)
			gridEXCell.getGridEX().TabElementChanging(true);			
		window.event.returnValue = false;
		window.event.cancelBubble = true;
		return false; 
	}
	function NotInList(value)	
	{	
		if(gridEXCell.getColumn().limitToList)
		{
			gridEXCell.getInnerCell().setAttribute("niv", value);
			gridEXCell.dataChanged = true;
			updateInnerCell(value, null); 
		}
	}
	function ValueChanged(value)
	{
		var args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, combo.getValue()); 
		var cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);  				
		if(cancel == null || !cancel)
		{
			if(value == null)
			{				
				gridEXCell.setText(combo.getDisplay(), combo.getImage()); 
				if(args != null)
					gridEXCell.setValue(args.getValue()); 
				else			
					gridEXCell.setValue(combo.getValue());
			}
			else
				gridEXCell.setValue(originalValue);				
			gridEXCell.getInnerCell().setAttribute("niv", null); 
			gridEXCell.getRow().ShowHeaderIndicator(true); 			
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]); 
		}
	}		
	function Show()
	{	
		var args = null;
		var cancel = null; 		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXEditingArgs(gridEXCell, gridEXCell.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("EditingCell", [args]); 
		}		
		if(cancel == null || !cancel)
		{	
			var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
			var _innerCell = gridEXCell.getInnerCell(); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(gridEXCell.getInnerSpan()) - getBorderLeftWidth(_innerCell);
			if(_rtl)
			{
				_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
				if(!gridEXCell.getGridEX().isHierarchicalGrid() && gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft >= 0)
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
				else if(!gridEXCell.isNewRecordTopCell() && gridEXCell.getGridEX().isHierarchicalGrid() && ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetLeft + gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetWidth) != gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth))
					_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}
			if(!gridEXCell.isNewRecordTopCell() && !_rtl)
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			else if(gridEXCell.isNewRecordTopCell() && _rtl)
				_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
			var _top = getPixelTop(_innerCell) + getPaddingTop(gridEXCell.getInnerSpan()) + getBorderTopWidth(_innerCell);
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 
			var _width = getRealWidth(_innerCell,false); 
			var _height = getRealHeight(_innerCell); 
			combo.setOwner(editManager); 			
			combo.setLeft(_left);
			combo.setTop(_top); 
			combo.setWidth(_width); 
			combo.setHeight(_height);
			combo.setStyle(_innerCell.currentStyle);
			combo.setCharacterCasing(gridEXCell.getColumn().characterCasing); 
			var _input = combo.getInnerTextBox(); 
			_input.style.padding = gridEXCell.getInnerSpan().currentStyle.padding;
			if(!gridEXCell.getRow().getTable().getUseColumnSets())
			{
				combo.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
				combo.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1)); 			
			}
			else
			{
				combo.setItemCSS(gridEXCell.getRow().getTable().getRowCss(33)); 
				combo.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(34)); 			
			}
			if(args != null)
			{
				if(gridEXCell.getInnerCell().getAttribute("niv") != null)
					combo.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
				else
					combo.setValue(args.getValue()); 
			}
			else
			{
				if(gridEXCell.getRow().getRowType() == "NewRecord")
				{
					if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
							combo.setValue(gridEXCell.getInnerCell().getAttribute("default"));
					else if(gridEXCell.getDataChanged())
						combo.setValue(gridEXCell.getValue());
					else if(gridEXCell.getInnerCell().getAttribute("niv") != null)
						combo.setValue(null, gridEXCell.getInnerCell().getAttribute("niv"));  
					else if(gridEXCell.getInnerCell().getAttribute("ind") != null && gridEXCell.getInnerCell().getAttribute("value") != null)
						combo.setValue(gridEXCell.getInnerCell().getAttribute("value")); 
				}
				else
				{
					if(gridEXCell.getInnerCell().getAttribute("niv") != null)	
						combo.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
					else
						combo.setValue(gridEXCell.getValue());
				}
			}			
			keepOriginalValues(args);			
			combo.Show(); 
			combo.Focus();		
		}
	}	
	var editManager = this;	
	return this; 
}