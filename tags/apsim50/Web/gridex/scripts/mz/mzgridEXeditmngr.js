//////////////////////////////////////////////////////////////////
// GridEX JavaScript MZ API 1.1.1009
// Copyright by Janus Systems S.A.
// 2002 - 2004
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
	this.ValueChanged = ValueChanged; 		
	this.Show = Show; 
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue(); 
		else
			originalValue = gridEXCell.getValue(); 
		
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0].getElementsByTagName("SPAN")[0];		
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 			
		}		
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				originalDisplay = _element.childNodes[0].data; 
		}
	}	
	function updateInnerCell(display)
	{
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _element = _innerSpan; 				
		if(_element.childNodes.length == 1)			
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 
		}		
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
			{
				if(display == null)
					_element.childNodes[0].data = ""; 
				else
					_element.childNodes[0].data = display; 
			}
		}
		else if(_element.childNodes.length == 0)
		{
			if(display == null)
				_element.appendChild(document.createTextNode(''));
			else
				_element.appendChild(document.createTextNode(' ' + display));
		}
	}		
	function ValueChanged()
	{
		var args = null; 
		var cancel = null;
		var dateAsText = calendar.getSelectedDateString();				
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dateAsText); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);
		}			
		if(cancel == null || !cancel)
		{				
			if(args != null)		
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(dateAsText);
				
			updateInnerCell(dateAsText);
			gridEXCell.getRow().ShowHeaderIndicator(true); 			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
				gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}	
	function EnterKeyDown()
	{		
		calendar.Hide();
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		updateInnerCell(originalDisplay); 
		gridEXCell.setValue(originalValue); 
		gridEXCell.UndoChanges(); 
		calendar.Hide(); 
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
			
		if(calendar.getInnerTextBox().value != originalDisplay)
			gridEXCell.getRow().ShowHeaderIndicator(true); 
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]);
	}
	function getCell() { return gridEXCell; }	
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
			var _innerCell = gridEXCell.getInnerCell(); 			
			var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 				
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());				
			var _width = _innerCell.offsetWidth - getPaddingRight(_innerSpan);
			var _height = _innerSpan.offsetHeight; //   - getPaddingBottom(_innerSpan);			
			calendar.setLeft(_left); 
			calendar.setTop(_top); 
			calendar.setWidth(_width); 
			calendar.setHeight(_height);
			calendar.setStyle(document.defaultView.getComputedStyle(_innerSpan, null));
			calendar.setOwner(editManager);			
			keepOriginalValues(args); 			
			var _input = calendar.getInnerTextBox(); 
			_input.style.paddingLeft = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-left"); 
			_input.style.paddingTop = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-top"); 
			_input.style.paddingRight = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-right"); 
			_input.style.paddingBottom = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-bottom"); 
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
	this.ValueChanged = ValueChanged; 	
	this.Show = Show; 	
	function ValueChanged()
	{
		var args = null; 
		var cancel = null; 		
		var dateAsText = calendar.getSelectedDateString(); 		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dateAsText); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]); 
		}			
		if(cancel == null || !cancel)
		{	
			if(args != null)		
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(dateAsText);
				
			if(args != null)
				updateInnerCell(args.getValue()); 
			else
				updateInnerCell(dateAsText); 
				
			gridEXCell.getRow().ShowHeaderIndicator(true); 			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
				gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]); 			
		}
		if(cancel != null && cancel == true)
			return true; 
	}	
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue(); 
		else
			originalValue = gridEXCell.getValue();
		
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0].getElementsByTagName("SPAN")[0];		
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 			
		}		
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				originalDisplay = _element.childNodes[0].data; 
		}
	}	
	function updateInnerCell(display)
	{
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0].getElementsByTagName("SPAN")[0]; 				
		if(_element.childNodes.length == 1)			
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 				
		}		
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				_element.childNodes[0].data = display; 
		}
		else if(_element.childNodes.length == 0)
			_element.appendChild(document.createTextNode(' ' + display));
	}		
	function EscKeyDown()
	{
		updateInnerCell(originalDisplay); 
		gridEXCell.setValue(originalValue);
		gridEXCell.UndoChanges(); 
		calendar.Hide(); 		
	}	
	function EnterKeyDown()
	{
		gridEXCell.ResumeEdit(); 
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
		else if(window.event.keyCode == 13)
			EnterKeyDown(); 
	}	
	function getCell()
	{
		return gridEXCell; 
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
			var _innerCell = gridEXCell.getInnerCell(); 
			var _innerSpan = _innerCell.getElementsByTagName("SPAN")[0]; 
			var _left = getPixelLeft(_innerSpan);
			_left -= getOffsetLeftForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);
			_top -= getOffsetTopForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 
			var _width = _innerCell.offsetWidth;
			var _height = _innerSpan.offsetHeight;			
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
			
		var _innerspan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]);
		var _element = _innerspan; 
		if(_element.childNodes.length == 0)
			return;
			
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 				
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG")
				originalImage = _element.childNodes[0].src; 
				
			if(_element.childNodes[1].nodeType == 3)
				originalDisplay = _element.childNodes[1].data; 
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				originalDisplay = _element.childNodes[0].data; 
		}		
	}	
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerspan = getInnerSpan(_innerCell.childNodes[0]);
		var _element = _innerspan; 
		if(_element.childNodes.length == 1)			
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG" && image != null)
			{
				_imgToUpdate = _element.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(_element.childNodes[1].nodeType == 3)
			{
				if(display == null)
					_element.childNodes[1].data = "";
				else
					_element.childNodes[1].data = display;
			}
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
			{
				if(display == null)
					_element.childNodes[0].data = ""; 
				else
					_element.childNodes[0].data = display; 
			}
		}
		else if(_element.childNodes.length == 0)
		{
			if(image != null)
			{				
				var _img = document.createElement("IMG"); 
				_img.align = "absmiddle"; 
				_img.border = "0"; 
				_img.height = "15px"; 
				_img.src = image; 
				_element.appendChild(_img); 
			}
			if(display == null)
				_element.appendChild(document.createTextNode('')); 
			else
				_element.appendChild(document.createTextNode(' ' + display));
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
			var _innerCell = gridEXCell.getInnerCell(); 
			var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
			var _left = getPixelLeft(_innerSpan) + getPaddingLeft(_innerSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());			
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = _innerCell.offsetWidth - getPaddingRight(_innerSpan);
			var _height = _innerCell.offsetHeight - getPaddingBottom(_innerSpan);			
			dropdown.setLeft(_left); 
			dropdown.setTop(_top); 
			dropdown.setWidth(_width); 
			dropdown.setHeight(_height); 
			dropdown.setStyle(document.defaultView.getComputedStyle(_innerCell, null));
			dropdown.setOwner(editManager); 			
			var _input = dropdown.getInnerTextBox(); 
			var _spanStyle = document.defaultView.getComputedStyle(_innerSpan, null); 
			_input.style.paddingLeft = _spanStyle.getPropertyValue("padding-left"); 
			_input.style.paddingTop = _spanStyle.getPropertyValue("padding-top"); 
			_input.style.paddingRight = _spanStyle.getPropertyValue("padding-right"); 
			_input.style.paddingBottom = _spanStyle.getPropertyValue("padding-bottom"); 			
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
						dropdown.setValue(gridEXCell.getInnerCell().getAttribute("value"));
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
			if(dropdown.getGridEX().getHtmlGridEX().getAttribute("fdd") != "1")
			{
				if(dropdown.getGridEX().getHtmlGridEX().getAttribute("lar") == gridEXCell.getRow().getID())
					return;
			}				
			var input = document.getElementsByName(gridEXCell.getGridEX().getID() + "_eventdata")[0]; 
			input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();
			gridEXCell.getGridEX().DoPostBack(null, "DropDown", true); 
			return false; 
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
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown();
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown();
		if(originalDisplay != dropdown.getInnerTextBox().value)
			gridEXCell.getRow().ShowHeaderIndicator(true); 
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]);
	}
	function ArrowDownKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MoveNext(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}	
	function ArrowUpKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}
	function EnterKeyDown()
	{
		dropdown.Hide(); 
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		if(originalValue != dropdown.getValue())
		{
			gridEXCell.setValue(dropdown.getValue()); 			
			updateInnerCell(originalDisplay, originalImage); 						
			gridEXCell.setValue(originalValue);
			gridEXCell.UndoChanges(); 			
		}
		gridEXCell.getInnerCell().setAttribute("niv", null);
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
	function NotInList(value)
	{
		if(gridEXCell.getColumn().limitToList)
		{
			gridEXCell.getInnerCell().setAttribute("niv", value); 
			gridEXCell.dataChanged = true; 
			updateInnerCell(value, originalImage);
		}
	}	
	function ValueChanged()
	{		
		var args = null; 
		var cancel = null; 
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dropdown.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);  // new Array(gridEXCell, originalValue, dropdown.getValue())
		}			
		if(cancel == null || !cancel)
		{
			if(args != null)		
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(dropdown.getValue());			
			
			updateInnerCell(dropdown.getDisplay(), originalImage);
			gridEXCell.getInnerCell().setAttribute("niv", null); 
			gridEXCell.getRow().ShowHeaderIndicator(true); 						
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);			
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
	function getCell()
	{
		return gridEXCell; 
	}		
	function keepOriginalValues(args)
	{
		if(args != null)
			originalValue = args.getValue();
		else
			originalValue = gridEXCell.getValue();
		
		var _innerspan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]); 
		var _element = _innerspan; 
		if(_element.childNodes.length == 0)
			return;
			
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 				
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG")
				originalImage = _element.childNodes[0].src; 
				
			if(_element.childNodes[1].nodeType == 3)
				originalDisplay = _element.childNodes[1].data; 
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				originalDisplay = _element.childNodes[0].data; 
		}		
	}	
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0].getElementsByTagName("SPAN")[0]; 
		if(_element.childNodes.length == 1)			
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG" && image != null)
			{
				_imgToUpdate = _element.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(_element.childNodes[1].nodeType == 3)
				_element.childNodes[1].data = display;
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				_element.childNodes[0].data = display; 
		}
		else if(_element.childNodes.length == 0)
		{
			if(image != null)
			{				
				var _img = document.createElement("IMG"); 
				_img.align = "absmiddle"; 
				_img.border = "0"; 
				_img.height = "15px"; 
				_img.src = image; 
				_element.appendChild(_img); 
			}						
			_element.appendChild(document.createTextNode(' ' + display));
		}
	}
	function DropDown()
	{
		if(gridEXCell.getGridEX().ddpb)
		{
			if(dropdown.getGridEX().getHtmlGridEX().getAttribute("fdd") != "1")
			{
				if(dropdown.getGridEX().getHtmlGridEX().getAttribute("lar") == gridEXCell.getRow().getID())
					return; 				
			}				
			var input = document.getElementsByName(gridEXCell.getGridEX().getID() + "_eventdata")[0]; 
			input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();
			gridEXCell.getGridEX().DoPostBack(null, "DropDown", true); 
			return false; 
		}
	}
	function EnterKeyDown()
	{
		gridEXCell.ResumeEdit();
	}	
	function EscKeyDown()
	{
		if(originalValue != dropdown.getValue())
		{
			gridEXCell.setValue(dropdown.getValue()); 			
			updateInnerCell(originalDisplay, originalImage); 						
			gridEXCell.setValue(originalValue);
			gridEXCell.UndoChanges(); 			
		}
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
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown();
	}
	function ArrowDownKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MoveNext(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}	
	function ArrowUpKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}
	function ValueChanged()
	{	
		var args = null; 
		var cancel = null;		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, dropdown.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);  // new Array(gridEXCell, originalValue, dropdown.getValue())
		}			
		if(cancel == null || !cancel)
		{			
			if(args != null)
				gridEXCell.setValue(args.getValue()); 
			else		
				gridEXCell.setValue(dropdown.getValue());
			
			updateInnerCell(dropdown.getDisplay(), originalImage);
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
			var _innerCell = gridEXCell.getInnerCell(); 
			var _innerSpan = getInnerSpan(_innerCell.childNodes[0]);
			var _left = getPixelLeft(_innerSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);			
			if(!gridEXCell.isNewRecordTopCell())			
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = _innerCell.offsetWidth;
			var _height = _innerCell.offsetHeight - getPaddingBottom(_innerSpan);						
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
		if(originalValue != textarea.getText())
		{		
			gridEXCell.setValue(originalValue); 
			gridEXCell.UndoChanges();			
			var _innerSpan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]); 
			var requiresNOBR = (_innerSpan.getElementsByTagName("NOBR").length == 1); 
			if(originalValue != null)
			{
				if(!requiresNOBR)
					_innerSpan.innerText = originalValue; 
				else
					_innerSpan.innerHTML = "<NOBR>" + originalValue + "</NOBR>"; 
			}
			else
			{
				if(!requiresNOBR)
					_innerSpan.innerText = ""; 
				else
					_innerSpan.innerHTML = "<NOBR></NOBR>"; 								
			}
		}				
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
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
	}	
	function Leaving()
	{
		textarea.Hide(); 
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
			var _innerCell = gridEXCell.getInnerCell();
			var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerSpan);
			_left -= getOffsetLeftForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);
			_top -= getOffsetTopForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = _innerCell.clientWidth - getPaddingRight(_innerCell.childNodes[0]); 
			var _height = _innerCell.clientHeight - getPaddingBottom(_innerCell.childNodes[0]);			
			textarea.setLeft(_left); 
			textarea.setTop(_top); 
			textarea.setWidth(_width); 
			textarea.setHeight(_height); 
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
				
			var _innerSpan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]);
			var requiresNOBR = (_innerSpan.getElementsByTagName("NOBR").length == 1); 
			if(!requiresNOBR)
				_innerSpan.innerText = newText;
			else
				_innerSpan.innerHTML = "<NOBR>" + newText + "</NOBR>";			
			gridEXCell.getRow().ShowHeaderIndicator(true); 
			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
				gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell] ); 		//	new Array(gridEXCell)
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
	this.TextChanged = TextChanged;	
	this.Show = Show; 	
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
			var _innerCell = gridEXCell.getInnerCell();			
			var _innerCellSpan = getInnerSpan(_innerCell.childNodes[0]);
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerCellSpan);
			_left -= getOffsetLeftForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCellSpan);
			_top -= getOffsetTopForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 			
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = _innerCell.offsetWidth - getPaddingRight(_innerCellSpan); 
			var _height = _innerCell.offsetHeight - getPaddingBottom(_innerCellSpan); 
			textbox.setLeft(_left); 
			textbox.setTop(_top); 
			textbox.setWidth(_width); 
			textbox.setHeight(_height); 
			textbox.setStyle(document.defaultView.getComputedStyle(_innerCell, null)); 			
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
			var _computedstyle = document.defaultView.getComputedStyle(_innerCellSpan, null); 			
			_input.style.paddingLeft = _computedstyle.getPropertyValue("padding-left"); 
			_input.style.paddingTop = _computedstyle.getPropertyValue("padding-top");
			_input.style.paddingRight = _computedstyle.getPropertyValue("padding-right");
			_input.style.paddingBottom = _computedstyle.getPropertyValue("padding-bottom");			
			textbox.setInputMask(gridEXCell.getColumn().getInputMask()); 	
			textbox.setInvalidValueAction(gridEXCell.getColumn().getInvalidValueAction()); 
			textbox.setInvalidValueMessage(gridEXCell.getColumn().getInvalidValueMessage());
			textbox.setMaxLength(gridEXCell.getColumn().getMaxLength());
			textbox.setOwner(editManager);			
			textbox.Show(); 
			textbox.Focus(); 
		}
	}
	function ApplyingInputMask()
	{
		return gridEXCell.getGridEX().FireEvent("ApplyingInputMask", [gridEXCell]); 
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
	function ArrowDownKeyDown()
	{
		textbox.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MoveNext(); 
	}	
	function ArrowUpKeyDown()
	{
		textbox.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious(); 
	}	
	function EnterKeyDown()
	{
		textbox.Hide(); 
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		textbox.Hide();						
		if(textbox.getText() != originalValue)
		{		
			gridEXCell.setValue(originalValue); 
			gridEXCell.UndoChanges(); 									
			var _innerspan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]); 
			var requiresNOBR = (_innerspan.getElementsByTagName("NOBR").length == 1); 
			if(originalValue != null)
			{
				if(!requiresNOBR)
					_innerspan.innerText = PasswordText(originalValue, gridEXCell.getColumn().getPasswordChar());
				else
					_innerspan.innerHTML = "<NOBR>" + PasswordText(originalValue, gridEXCell.getColumn().getPasswordChar()) + "</NOBR>"; 				
			}
			else	
			{
				if(!requiresNOBR)
					_innerspan.innerText = ""; 
				else
					_innerspan.innerHTML = "<NOBR></NOBR>"; 						
			}
			gridEXCell.getInnerCell().setAttribute("text", originalValue);
		}				
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();				
	}	
	function Leaving()
	{
		textbox.Hide(); 		
	}	
	function TabKeyDown()	
	{								
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
				
			var _innerspan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]);
			var requiresNOBR = _innerspan.getElementsByTagName("NOBR"); 
			if(!requiresNOBR)
				_innerspan.innerText = PasswordText(newText,gridEXCell.getColumn().getPasswordChar());
			else
				_innerspan.innerHTML = "<NOBR>" + PasswordText(newText,gridEXCell.getColumn().getPasswordChar()) + "</NOBR>";
			
			gridEXCell.getInnerCell().setAttribute("text",  newText);
			gridEXCell.getRow().ShowHeaderIndicator(true);			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
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
	this.TextChanged = TextChanged;	
	this.Show = Show; 	
	function ApplyingInputMask()
	{
		return gridEXCell.getGridEX().FireEvent("ApplyingInputMask", [gridEXCell]); 
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
	
			var _innerCell = gridEXCell.getInnerCell();			
			var _innerCellSpan = getInnerSpan(_innerCell.childNodes[0]);
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerCellSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX());
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCellSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX());
			var _width = _innerCell.offsetWidth - getPaddingRight(_innerCellSpan); 
			var _height = _innerCell.offsetHeight  - getPaddingBottom(_innerCellSpan); 
			
			textbox.setLeft(_left); 
			textbox.setTop(_top); 
			textbox.setWidth(_width); 
			textbox.setHeight(_height); 
			textbox.setStyle(document.defaultView.getComputedStyle(_innerCell, null)); 			
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
			var _computedstyle = document.defaultView.getComputedStyle(_innerCellSpan, null); 			
			_input.style.paddingLeft = _computedstyle.getPropertyValue("padding-left"); 
			_input.style.paddingTop = _computedstyle.getPropertyValue("padding-top");
			_input.style.paddingRight = _computedstyle.getPropertyValue("padding-right");
			_input.style.paddingBottom = _computedstyle.getPropertyValue("padding-bottom");			
			textbox.setInputMask(gridEXCell.getColumn().getInputMask()); 	
			textbox.setInvalidValueAction(gridEXCell.getColumn().getInvalidValueAction()); 
			textbox.setInvalidValueMessage(gridEXCell.getColumn().getInvalidValueMessage());
			textbox.setMaxLength(gridEXCell.getColumn().getMaxLength());
			textbox.setOwner(editManager);
			textbox.Show(); 
			textbox.Focus(); 
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
	function ArrowDownKeyDown()
	{
		textbox.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getGridEX().MoveNext(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}	
	function ArrowUpKeyDown()
	{
		textbox.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious();
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}	
	function EnterKeyDown()
	{
		textbox.Hide(); 
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		textbox.Hide();						
		if(textbox.getText() != originalValue)
		{		
			gridEXCell.setValue(originalValue); 
			gridEXCell.UndoChanges(); 									
			var _innerspan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]); 
			var requiresNOBR = (_innerspan.getElementsByTagName("NOBR").length == 1); 
			if(originalValue != null)
			{
				if(!requiresNOBR)
					_innerspan.innerText = originalValue;
				else
					_innerspan.innerHTML = "<NOBR>" + originalValue + "</NOBR>"; 				
			}
			else	
			{
				if(!requiresNOBR)
					_innerspan.innerText = ""; 
				else
					_innerspan.innerHTML = "<NOBR></NOBR>"; 						
			}
		}				
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();				
	}	
	function Leaving()
	{
		textbox.Hide(); 		
	}	
	function TabKeyDown()	
	{
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
				
			var _innerspan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]);
			var requiresNOBR = _innerspan.getElementsByTagName("NOBR"); 
			if(!requiresNOBR)
				_innerspan.innerText = newText;
			else
				_innerspan.innerHTML = "<NOBR>" + newText + "</NOBR>";
			
			gridEXCell.getRow().ShowHeaderIndicator(true);			
			if(gridEXCell.getGridEX().getClientEventsCount() > 0)
				gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}	
	var editManager = this; 	
	return this; 
}
var _imgToUpdate = null; 
function updateImage(src)
{
	if(_imgToUpdate != null)
		_imgToUpdate.src = src; 
		
	_imgToUpdate = null; 
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
	function getCell()
	{
		return gridEXCell; 
	}	
	function DropDown()
	{
		if(valueList.ddpb)
		{
			if(valueList.getInnerList().getAttribute("fdd") == "1" || valueList.getInnerList().getAttribute("lar") != gridEXCell.getRow().getID())
			{
				var input = document.getElementsByName(gridEXCell.getGridEX().getID() + "_eventdata")[0];
				input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();
				gridEXCell.getGridEX().DoPostBack(null, "DropDown", true);
				return false;
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
	function EnterKeyDown()
	{
		gridEXCell.ResumeEdit(); 
	}	
	function EscKeyDown()
	{
		if(originalValue != gridEXCell.getValue())
		{						
			gridEXCell.setValue(originalValue); 
			gridEXCell.UndoChanges(); 			
			updateInnerCell(originalDisplay, originalImage);
		}		
		valueList.Hide();
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
		if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
	}
	function Leaving()
	{
		
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
				
			updateInnerCell(valueList.getDisplay(), valueList.getImage());				
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
		
		var _element = gridEXCell.getInnerCell().childNodes[0].getElementsByTagName("SPAN")[0]; 
		if(_element.childNodes.length == 0)
			return;
			
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 				
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG")
				originalImage = _element.childNodes[0].src; 
				
			if(_element.childNodes[1].nodeType == 3)
				originalDisplay = _element.childNodes[1].data; 
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				originalDisplay = _element.childNodes[0].data; 
		}		
	}	
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0].getElementsByTagName("SPAN")[0]; 
		if(_element.childNodes.length == 1)			
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG" && image != null)
			{
				_imgToUpdate = _element.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(_element.childNodes[1].nodeType == 3)
				_element.childNodes[1].data = display;			
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				_element.childNodes[0].data = display; 
		}
		else if(_element.childNodes.length == 0)
		{
			if(image != null)
			{				
				var _img = document.createElement("IMG"); 
				_img.align = "absmiddle"; 
				_img.border = "0"; 
				_img.height = "15px"; 
				_img.src = image; 
				_element.appendChild(_img); 
			}						
			_element.appendChild(document.createTextNode(' ' + display));
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
			var _innerCell = gridEXCell.getInnerCell(); 
			var _innerSpan = _innerCell.childNodes[0].getElementsByTagName("SPAN")[0]; 
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerSpan);
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 
			var _width = _innerCell.offsetWidth - getPaddingRight(_innerSpan);
			var _height = _innerCell.offsetHeight - getPaddingBottom(_innerSpan);	
			valueList.setOwner(editManager); 			
			valueList.setLeft(_left);		
			valueList.setTop(_top); 
			valueList.setWidth(_width); 
			valueList.setHeight(_height);			
			valueList.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
			valueList.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1)); 					
			keepOriginalValues(args);			
			valueList.Show(); 			
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
		
		var _innerspan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]); 
		var _element = _innerspan; 
		if(_element.childNodes.length == 0)
			return;		
			
		if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 				
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG")
				originalImage = _element.childNodes[0].src; 
				
			if(_element.childNodes[1].nodeType == 3)
				originalDisplay = _element.childNodes[1].data; 
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
				originalDisplay = _element.childNodes[0].data; 
		}		
	}	
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerspan = getInnerSpan(_innerCell.childNodes[0]); 
		var _element = _innerspan;
		if(_element.childNodes.length == 1)			
		{
			if(_element.childNodes[0].tagName == "NOBR")
				_element = _element.childNodes[0]; 
		}		
		if(_element.childNodes.length == 2)
		{
			if(_element.childNodes[0].nodeType == 1 && _element.childNodes[0].tagName == "IMG")
			{
				_imgToUpdate = _element.childNodes[0]; 
				window.setTimeout("updateImage('" + image + "')", 1);
			}				
			if(_element.childNodes[1].nodeType == 3)
			{
				if(display == null)
					_element.childNodes[1].data = "";
				else
					_element.childNodes[1].data = display;
			}
		}
		else if(_element.childNodes.length == 1)
		{
			if(_element.childNodes[0].nodeType == 3)
			{
				if(display == null)
					_element.childNodes[0].data = ""; 
				else
					_element.childNodes[0].data = display; 
			}
		}
		else if(_element.childNodes.length == 0)
		{
			if(image != null)
			{				
				var _img = document.createElement("IMG"); 
				_img.align = "absmiddle"; 
				_img.border = "0"; 
				_img.height = "15px"; 
				_img.src = image; 
				_element.appendChild(_img); 
			}
			if(display == null)
				_element.appendChild(document.createTextNode('')); 
			else
				_element.appendChild(document.createTextNode(' ' + display));
		}
	}		
	function DropDown()
	{
		if(combo.ddpb)
		{
			if(combo.getInnerCombo().getAttribute("fdd") == "1" || combo.getInnerCombo().getAttribute("lar") != gridEXCell.getRow().getID())
			{
				var input = document.getElementById(gridEXCell.getGridEX().getID() + "_eventdata");
				input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();				
				gridEXCell.getGridEX().DoPostBack(null, "DropDown", true);
				return false;
			}			
		}
		if(gridEXCell.getRow().getRowType() == "NewRecord")
		{
			if(!gridEXCell.getDataChanged() && gridEXCell.getInnerCell().getAttribute("default") != null)
				combo.setValue(gridEXCell.getInnerCell().getAttribute("default"));
			else if(gridEXCell.getDataChanged())
				combo.setValue(gridEXCell.getValue()); 
			else if(gridEXCell.getInnerCell().getAttribute("ind") != null && gridEXCell.getInnerCell().getAttribute("value") != null)
				combo.setValue(gridEXCell.getInnerCell().getAttribute("value"));
		}
		else
			combo.setValue(gridEXCell.getValue());
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
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 
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
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}	
	function ArrowUpKeyDown()
	{
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}
	function EnterKeyDown() { gridEXCell.ResumeEdit(); }	
	function EscKeyDown()
	{	
		if(originalValue != combo.getValue())
		{	
			gridEXCell.setValue(originalValue); 
			gridEXCell.UndoChanges(); 			
			updateInnerCell(originalDisplay, originalImage);					
		}
		gridEXCell.getInnerCell().setAttribute("niv", null);
		combo.Hide(); 
	}	
	function Leaving() { combo.Hide(); }
	function NotInList(value)	
	{	
		if(gridEXCell.getColumn().limitToList)
		{
			gridEXCell.getInnerCell().setAttribute("niv", value);
			gridEXCell.dataChanged = true;
			updateInnerCell(value, null); 
		}
	}
	function ValueChanged()
	{		
		var args = null; 
		var cancel = null; 		
		if(gridEXCell.getGridEX().getClientEventsCount() > 0)
		{
			args = new GridEXUpdatingCellArgs(gridEXCell, originalValue, combo.getValue()); 
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]); 
		}			
		if(cancel == null || !cancel)
		{			
			updateInnerCell(combo.getDisplay(), combo.getImage()); 			
			if(args != null)
				gridEXCell.setValue(args.getValue()); 
			else			
				gridEXCell.setValue(combo.getValue());
				
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
			var _innerCell = gridEXCell.getInnerCell();
			var _innerCellSpan = getInnerSpan(_innerCell.childNodes[0]);
			var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerCellSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_left -= getHorizontalScrollOffset(gridEXCell.getGridEX()); 
			var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCellSpan);			
			if(!gridEXCell.isNewRecordTopCell())
				_top -= getVerticalScrollOffset(gridEXCell.getGridEX()); 
			var _width = _innerCell.offsetWidth - getPaddingRight(_innerCellSpan);
			var _height = _innerCell.offsetHeight - getPaddingBottom(_innerCellSpan); 
			combo.setOwner(editManager); 			
			combo.setLeft(_left);
			combo.setTop(_top); 
			combo.setWidth(_width); 
			combo.setHeight(_height);
			combo.setStyle(document.defaultView.getComputedStyle(_innerCell, null));
			var _input = combo.getInnerTextBox(); 
			var _computedstyle = document.defaultView.getComputedStyle(_innerCellSpan, null);			
			_input.style.paddingLeft = _computedstyle.getPropertyValue("padding-left");
			_input.style.paddingTop = _computedstyle.getPropertyValue("padding-top");
			_input.style.paddingRight = _computedstyle.getPropertyValue("padding-right");
			_input.style.paddingBottom = _computedstyle.getPropertyValue("padding-bottom");
			combo.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
			combo.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1)); 			
			if(args != null)
			{				
				if(gridEXCell.getInnerCell().getAttribute("niv") != null && gridEXCell.getInnerCell().getAttribute("niv") != "")
					combo.setValue(null, gridEXCell.getInnerCell().getAttribute("niv"));
				else
					combo.setValue(args.getValue()); 
			}
			else			
			{
				if(gridEXCell.getInnerCell().getAttribute("niv") != null)						
					combo.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
				else					
					combo.setValue(gridEXCell.getValue());
			}
			keepOriginalValues(args);
			combo.Show(); 
			combo.Focus();		
		}
	}	
	var editManager = this;	
	return this; 
}