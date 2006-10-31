/////////////////////////////////////////////////////////////////////
// GridEX JavaScript MZ API 1.1.1009
// Copyright by Janus Systems S.A.
// 2002 - 2004
/////////////////////////////////////////////////////////////////////
function ResetFilterCommand()
{
	var divFilter = null; 
	var initialized = false; 
	var currentManager = null; 
	this.getInnerHtml = getInnerHtml; 
	this.getManager = getManager; 
	this.setManager = setManager; 
	this.Hide = Hide;
	this.Init = Init;
	this.Show = Show; 	
	function getInnerHtml()
	{
		return divFilter; 
	}
	function getManager()
	{
		return currentManager; 
	}
	function setManager(manager)
	{
		currentManager = manager;
	}
	function Init(clientID)
	{	
		divFilter = document.getElementById(clientID + "_resetFilter"); 
		if(divFilter == null)
			throw Error("unable to find client reset fiter command");
			
		divFilter.addEventListener("mousedown", resetFilter_onmousedown, false); 
	}
	function Hide()
	{		
		divFilter.style.visibility = "hidden"; 
		currentManager = null; 
	}
	function Show(left, top, height)
	{
		divFilter.style.visibility = "visible"; 
		divFilter.style.left = (left) + "px"; 
		divFilter.style.top = (top) + "px";
		divFilter.style.height = (height) + "px";
	}
	function resetFilter_onmousedown()
	{			
		if(currentManager != null)
			currentManager.ResetFilter(); 
			
		window.event.returnValue = false; 
	}
	return this; 
}
function GridEXFilterCheckBoxManager(cell)
{
	var checkbox = cell.getInnerCell().getElementsByTagName("INPUT")[0]; 	
	if(checkbox == null || checkbox.type != "checkbox")
		throw new Error("invalid check box object");
		
	var gridEXCell = cell;	
	var resetCommand = null; 
	this.Hide = Hide;
	this.Show = Show;		
	this.ResetFilter= ResetFilter; 
	function Hide()
	{
		resetCommand.Hide(); 
	}
	function Show()
	{			
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager);
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _left = getPixelLeft(_innerCell);
		_left -= getOffsetLeftForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);
		_top -= getOffsetTopForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1;
		var _height = _innerSpan.offsetHeight;	
		resetCommand.Show(_left + _width + 1, _top, _height); 
	}	
	function ResetFilter()
	{
		checkbox.checked = false; 
		gridEXCell.getInnerCell().setAttribute("remfilter", -1); 
		gridEXCell.setChecked(); 		
		gridEXCell.ResumeFilter(); 
		Hide(); 
	}
	function filter_onkeydown()
	{
		if(window.event.keyCode == 13)
			gridEXCell.ResumeFilter(); 
	}
	checkbox.addEventListener("keydown", filter_onkeydown, false); 
	var filterManager = this; 
	return this;
}
function GridEXFilterComboManager(cell, actAsEdit)
{
	var gridEXCell = cell; 
	var combo = null; 		
	var resetCommand = null; 	
	if(actAsEdit != null && actAsEdit == true)
		combo = gridEXCell.getGridEX().getEditControl(5, gridEXCell.getColumn().getClientID() + "_Combo"); 
	else
		combo = new GridEXCombo(gridEXCell.getColumn().getFilterListID());
		
	if(combo == null)
		throw Error("invalid operation exception: filter combo is null or invalid");
		
	if(actAsEdit != null && actAsEdit == true)
	{
		combo.setAutoComplete(true); 
		combo.setCompareTarget(1); 
	}
	else
	{		
		combo.setAutoComplete(false); 
		combo.setCompareTarget(2); 
	}
	this.getCell = getCell;	
	this.DropDown = DropDown; 	
	this.KeyDown = KeyDown;
	this.KeyUp = KeyUp; 
	this.Leaving = Leaving; 
	this.NotInList = NotInList; 
	this.ValueChanged = ValueChanged; 	
	this.ResetFilter = ResetFilter; 
	this.Hide = Hide; 
	this.Show = Show;	
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
		combo.setValue(gridEXCell.getValue()); 
	}
	function EnterKeyDown()
	{
		gridEXCell.ResumeFilter(); 
	}
	function EscKeyDown()
	{
		Hide();
		gridEXCell.getInnerCell().setAttribute("niv", null); 
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
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
	}
	function Leaving()
	{
		Hide(); 
	}
	function Hide()
	{
		combo.Hide(); 
		if(resetCommand != null)
			resetCommand.Hide();
	}
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager);		
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _left = getPixelLeft(_innerCell);
		_left -= getOffsetLeftForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);
		_top -= getOffsetTopForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1;
		var _height = _innerSpan.offsetHeight;
		combo.setOwner(filterManager); 			
		combo.setLeft(_left);
		combo.setTop(_top); 
		combo.setWidth(_width); 
		combo.setHeight(_height);
		combo.setStyle(document.defaultView.getComputedStyle(_innerCell, null));
		var _input = combo.getInnerTextBox(); 
		var _spanStyle = document.defaultView.getComputedStyle(_innerCellSpan, null); 
		_input.style.paddingLeft = _spanStyle.getPropertyValue("padding-left"); 
		_input.style.paddingTop = _spanStyle.getPropertyValue("padding-top"); 
		_input.style.paddingRight = _spanStyle.getPropertyValue("padding-right"); 
		_input.style.paddingBottom = _spanStyle.getPropertyValue("padding-bottom"); 
		combo.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
		combo.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1));
		if(gridEXCell.getInnerCell().getAttribute("niv") != null)
			combo.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
		else
			combo.setValue(gridEXCell.getValue());
		originalValue = gridEXCell.getValue(); 
		resetCommand.Show(_left + _width + 1, _top, _height); 
		combo.Show();
		combo.Focus();
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
	var originalValue = null; 
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
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()	
	{
		if(gridEXCell.getValue() != null || gridEXCell.getInnerCell().getAttribute("niv") != null)
		{		
			updateInnerCell("", null); 
			combo.setValue(null); 
			gridEXCell.getInnerCell().setAttribute("niv", null); 
			gridEXCell.ResumeFilter(); 
			Hide();
		}
	}
	function getCell()
	{
		return gridEXCell; 
	}	
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = getInnerSpan(_innerCell.childNodes[0]); 
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
	var filterManager = this; 
	return this;
}
function GridEXFilterListManager(cell, actAsEdit)
{
	var gridEXCell = cell; 
	var resetCommand = null; 
	var valueList = null; 
	if(actAsEdit != null && actAsEdit == true)
		valueList = gridEXCell.getGridEX().getEditControl(6, gridEXCell.getColumn().getClientID() + "_ValueList");
	else
		valueList = new GridEXValueList(cell.getColumn().getFilterListID()); 
	
	if(valueList == null)
		throw Error("invalid operation exception: filter list is null or invalid"); 
		
	if(actAsEdit != null && actAsEdit == true)	
		valueList.setCompareTarget(1); 
	else		
		valueList.setCompareTarget(2);  
		
	this.getCell = getCell; 		
	this.DropDown = DropDown; 
	this.EnterKeyDown = EnterKeyDown; 
	this.EscKeyDown = EscKeyDown; 
	this.Leaving = Leaving; 
	this.ValueChanged = ValueChanged;
	this.ResetFilter = ResetFilter; 
	this.Show = Show; 	
	function getCell() { return gridEXCell;}	
	function Hide()
	{
		valueList.Hide(); 
		if(resetCommand != null)
			resetCommand.Hide(); 
	}
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 		
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _left = getPixelLeft(_innerCell);
		_left -= getOffsetLeftForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);		
		_top -= getOffsetTopForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.offsetHeight; // _innerSpan.offsetHeight;
		originalValue = gridEXCell.getValue(); 		
		valueList.setOwner(filterManager); 			
		valueList.setLeft(_left);
		valueList.setTop(_top); 
		valueList.setWidth(_width); 		
		valueList.setHeight(_height);	
		valueList.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
		valueList.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1)); 
		valueList.Show();		
		resetCommand.Show(_left+_width + 1, _top, _height); 
		valueList.Focus(); 		
	}
	var originalValue = null; 
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = getInnerSpan(_innerCell.childNodes[0]); 
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
	function EnterKeyDown()
	{
		gridEXCell.ResumeFilter(); 
	}
	function EscKeyDown()
	{			
		Hide(); 
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
		valueList.setValue(gridEXCell.getValue()); 
	}	
	function Leaving()
	{
		Hide(); 		
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
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}		
	function ResetFilter()
	{
		if(gridEXCell.getValue() != null)
		{
			updateInnerCell("", null);
			gridEXCell.setValue(null); 
			gridEXCell.ResumeFilter(); 
			Hide();
		}
	}
	var filterManager = this; 
	return this;
}
function GridEXFilterDropDownManager(cell)
{
	var gridEXCell = cell; 
	var dropdown = gridEXCell.getGridEX().getEditControl(8, gridEXCell.getColumn().getDropDownID()); 
	var originalImage = null; 
	var resetCommand = null; 	
	this.getCell = getCell; 
	this.DropDown = DropDown;
	this.KeyDown = KeyDown;	
	this.Leaving = Leaving; 
	this.ResetFilter = ResetFilter; 
	this.ValueChanged = ValueChanged;	
	this.Show = Show; 			
	function getCell()
	{
		return gridEXCell; 
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
		if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown(); 			
	}
	function EnterKeyDown()
	{
		gridEXCell.ResumeFilter(); 
	}
	function EscKeyDown()
	{
		Hide(); 		
	}
	function Leaving()
	{
		if(resetCommand != null && resetCommand.getManager() == filterManager)
		{			
			if(document.activeElement == resetCommand.getInnerHtml() || resetCommand.getInnerHtml().contains(document.activeElement))
				return true; 
		}		
		Hide(); 
	}
	function ValueChanged()
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
			if(args != null)
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(dropdown.getValue());
				
			updateInnerCell(dropdown.getDisplay(), originalImage);
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()
	{		
		if(gridEXCell.getValue() != null)
		{
			updateInnerCell("", null);
			gridEXCell.setValue(null); 
			gridEXCell.ResumeFilter(); 
			Hide();
		}
	}
	function updateInnerCell(display, image)
	{		
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = getInnerSpan(_innerCell.childNodes[0]);
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
	function Hide()
	{
		dropdown.Hide(); 
		if(resetCommand != null)
			resetCommand.Hide();
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]);
		var _left = getPixelLeft(_innerSpan);		
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.offsetHeight - getPaddingBottom(_innerSpan); 
		originalValue = gridEXCell.getValue(); 
		dropdown.setLeft(_left); 
		dropdown.setTop(_top); 
		dropdown.setWidth(_width); 
		dropdown.setHeight(_height); 			
		dropdown.setOwner(filterManager);
		dropdown.setValue(gridEXCell.getValue()); 		
		resetCommand.Show(_left+_width+1, _top, _height); 
		dropdown.Show(); 
	}	
	var filterManager = this;
	return this; 
}
function GridEXFilterComboDropDownManager(cell)
{
	var gridEXCell = cell;
	var dropdown = gridEXCell.getGridEX().getEditControl(7, gridEXCell.getColumn().getDropDownID()); 		
	var originalImage = null; 
	var resetCommand = null; 	
	this.getCell = getCell; 
	this.DropDown = DropDown; 
	this.KeyDown = KeyDown;
	this.KeyUp = KeyUp; 
	this.Leaving = Leaving;
	this.NotInList = NotInList;	
	this.ValueChanged = ValueChanged; 
	this.ResetFilter = ResetFilter; 
	this.Show = Show;		
	function getCell() { return gridEXCell; }
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
	}
	function KeyUp() { gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); }
	function Leaving()
	{		
		if(resetCommand != null && resetCommand.getManager() == filterManager)
		{
			if(document.activeElement == resetCommand.getInnerHtml() || resetCommand.getInnerHtml().contains(document.activeElement))
				return true;
		}
		Hide(); 
	}
	function Hide()
	{
		dropdown.Hide();
		resetCommand.Hide(); 
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
			cancel = gridEXCell.getGridEX().FireEvent("UpdatingCell", [args]);  
		}			
		if(cancel == null || !cancel)
		{
			if(args != null)
				gridEXCell.setValue(args.getValue()); 
			else
				gridEXCell.setValue(dropdown.getValue());
				
			updateInnerCell(dropdown.getDisplay(), originalImage);
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()
	{
		if(gridEXCell.getValue() != null || gridEXCell.getInnerCell().getAttribute("niv") != null)
		{
			updateInnerCell("", null); 
			gridEXCell.setValue(null); 
			gridEXCell.getInnerCell().setAttribute("niv", null); 
			gridEXCell.ResumeFilter(); 
			Hide(); 
		}
	}
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 		
		var _element = getInnerSpan(_innerCell.childNodes[0]); 
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
	function EnterKeyDown()
	{	
		dropdown.Hide(); 
		gridEXCell.ResumeFilter(); 
	}	
	function EscKeyDown()
	{		
		dropdown.Hide();
		gridEXCell.getInnerCell().setAttribute("niv", null);
	}
	var originalValue = null; 
	function Show()
	{		
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager);
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _left = getPixelLeft(_innerSpan);		
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.offsetHeight - getPaddingBottom(_innerSpan); 
		originalValue = gridEXCell.getValue(); 
		dropdown.setLeft(_left); 
		dropdown.setTop(_top); 
		dropdown.setWidth(_width); 
		dropdown.setHeight(_height); 
		dropdown.setStyle(document.defaultView.getComputedStyle(_innerCell, null));
		dropdown.setOwner(filterManager);
		var _input = dropdown.getInnerTextBox(); 
		var _spanStyle = document.defaultView.getComputedStyle(_innerSpan, null);
		_input.style.paddingLeft = _spanStyle.getPropertyValue("padding-left"); 
		_input.style.paddingTop = _spanStyle.getPropertyValue("padding-top");
		_input.style.paddingRight = _spanStyle.getPropertyValue("padding-right"); 
		_input.style.paddingBottom = _spanStyle.getPropertyValue("padding-bottom"); 
		if(gridEXCell.getInnerCell().getAttribute("niv") != null)
			dropdown.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
		else
			dropdown.setValue(gridEXCell.getValue());		
		resetCommand.Show(_left+_width+1, _top, _height);
		dropdown.Show();
		// dropdown.Focus(); 
	}
	var filterManager = this;
	return this; 
}
function GridEXFilterCalendarComboManager(cell)
{
	var gridEXCell = cell; 
	var calendar = gridEXCell.getGridEX().getEditControl(4);
	var resetCommand = null;
	this.getCell = getCell;
	this.KeyDown = KeyDown;
	this.KeyUp = KeyUp; 
	this.ResetFilter = ResetFilter; 
	this.ValueChanged = ValueChanged; 	
	this.Show = Show; 
	function getCell()
	{
		return gridEXCell;
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
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
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
				_element.childNodes[0].data = display; 
		}
		else if(_element.childNodes.length == 0)
			_element.appendChild(document.createTextNode(' ' + display));
	}
	function Hide()
	{		
		calendar.Hide(); 
		if(resetCommand != null)
			resetCommand.Hide(); 
	}
	function EnterKeyDown()
	{	
		Hide(); 
		gridEXCell.ResumeFilter(); 
	}	
	function EscKeyDown()
	{
		Hide(); 
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
				
			if(args != null)
				updateInnerCell(args.getValue());
			else
				updateInnerCell(dateAsText);
				
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()
	{
		if(gridEXCell.getValue() != null)
		{
			gridEXCell.setValue(null); 
			updateInnerCell(""); 
			gridEXCell.ResumeFilter(); 
			Hide(); 
		}
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerSpan);		
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);		
		var _width = _innerCell.offsetWidth - getPaddingRight(_innerSpan) - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.offsetHeight - getPaddingBottom(_innerSpan); 
		originalValue = gridEXCell.getValue(); 
		calendar.setLeft(_left);
		calendar.setTop(_top); 
		calendar.setWidth(_width); 
		calendar.setHeight(_height);			
		calendar.setStyle(document.defaultView.getComputedStyle(_innerSpan, null));
		calendar.setOwner(filterManager);
		calendar.setSelectedDate(gridEXCell.getValue());
		var _input = calendar.getInnerTextBox(); 
		_input.style.paddingLeft = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-left"); 
		_input.style.paddingTop = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-top"); 
		_input.style.paddingRight = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-right"); 
		_input.style.paddingBottom = document.defaultView.getComputedStyle(_innerSpan, null).getPropertyValue("padding-bottom"); 
		resetCommand.Show(_left+_width+1,_top,_height); 
		calendar.Show();
	}	
	var filterManager = this;
	return this; 
}
function GridEXFilterCalendarDropDownManager(cell)
{
	var gridEXCell = cell; 
	var calendar = gridEXCell.getGridEX().getEditControl(3); 
	var resetCommand = null; 
	this.getCell = getCell; 
	this.KeyDown = KeyDown; 
	this.Leaving = Leaving; 
	this.ResetFilter = ResetFilter; 
	this.ValueChanged = ValueChanged; 	
	this.Show = Show;
	this.Hide = Hide;
	function getCell() { return gridEXCell; }
	function KeyDown()
	{
		if(window.event.keyCode == 13)
			EnterKeyDown(); 
		else if(window.event.keyCode == 27)
			EscKeyDown(); 		
	}	
	function updateInnerCell(display)
	{
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = getInnerSpan(_innerCell.childNodes[0]);
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
	function EnterKeyDown() { gridEXCell.ResumeFilter(); }	
	function EscKeyDown() { Hide();  }	
	function Hide()
	{		
		calendar.Hide(); 
		if(resetCommand != null)
			resetCommand.Hide(); 
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _innerCell = gridEXCell.getInnerCell(); 
		var _innerSpan = getInnerSpan(_innerCell.childNodes[0]);
		var _left = getPixelLeft(_innerSpan) + getPaddingLeft(_innerSpan);		
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerSpan);		
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.offsetHeight  - getPaddingBottom(_innerSpan);
		originalValue = gridEXCell.getValue(); 
		calendar.setLeft(_left); 
		calendar.setTop(_top); 
		calendar.setWidth(_width); 
		calendar.setHeight(_height);			
		calendar.setOwner(filterManager);		
		calendar.setSelectedDate(null); 		
		resetCommand.Show(_left+_width+1,_top,_height); 
		calendar.Show(); 		
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
			
			if(args != null)
				updateInnerCell(args.getValue()); 
			else
				updateInnerCell(dateAsText); 
				
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
		if(cancel != null && cancel == true)
			return true; 
	}
	function ResetFilter()
	{
		if(gridEXCell.getValue() != null)
		{
			updateInnerCell(""); 
			gridEXCell.setValue(null);
			gridEXCell.ResumeFilter(); 
			Hide(); 
		}
	}
	function Leaving()
	{
		if(resetCommand != null && resetCommand.getManager() == filterManager)
		{			
			if(document.activeElement == resetCommand.getInnerHtml() || resetCommand.getInnerHtml().contains(document.activeElement))
				return true; 
		}		
		Hide(); 
	}
	var filterManager = this; 
	return this; 
}
function GridEXFilterPasswordManager(cell, sameAsEdit)
{
	var gridEXCell = cell; 
	var actAsEditTextBox = sameAsEdit;
	var resetCommand = null; 
	var textbox = gridEXCell.getGridEX().getEditControl(9); 	
	this.KeyDown = KeyDown;
	this.Leaving = Leaving;
	this.ResetFilter = ResetFilter; 
	this.TextChanged = TextChanged;
	this.Hide = Hide; 
	this.Show = Show; 	
	function KeyDown()
	{
		if(window.event.keyCode == 27)
			EscKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown();
		else if(window.event.keyCode == 13)
			EnterKeyDown(); 
	}	
	function Leaving()
	{
		Hide(); 
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
				
			var _innerCellSpan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]); 		
			_innerCellSpan.innerText = PasswordText(newText, gridEXCell.getColumn().getPasswordChar());
			gridEXCell.getInnerCell().setAttribute("text", newText); 
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()
	{
		if(gridEXCell.getValue() != null)
		{
			gridEXCell.setValue(null);
			var _innerCellSpan = getInnerSpan(gridEXCell.getInnerCell().chilNodes[0]);
			_innerCellSpan.innerText = "";
			gridEXCell.getInnerCell().setAttribute("text", null); 
			gridEXCell.ResumeFilter(); 
			Hide(); 
		}
	}
	function EnterKeyDown()
	{
		Hide(); 
		gridEXCell.ResumeFilter(); 
	}	
	function EscKeyDown()
	{
		Hide(); 
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
	function Hide()
	{
		textbox.Hide(); 
		if(resetCommand != null)
			resetCommand.Hide(); 
	}
	var originalValue = null; 	
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _innerCell = gridEXCell.getInnerCell();		
		var _innerCellSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerCellSpan);				
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCellSpan);
		var _width = _innerCell.offsetWidth - getPaddingRight(_innerCellSpan) - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.offsetHeight - getPaddingBottom(_innerCellSpan);
		originalValue = gridEXCell.getValue(); 
		textbox.setLeft(_left); 
		textbox.setTop(_top); 		
		textbox.setWidth(_width); 
		textbox.setHeight(_height); 
		textbox.setStyle(document.defaultView.getComputedStyle(_innerCell, null));
		textbox.setText(gridEXCell.getValue()); 		
		var _input = textbox.getInnerHTML(); 		
		var _spanStyle = document.defaultView.getComputedStyle(_innerCellSpan, null); 
		_input.style.paddingLeft = _spanStyle.getPropertyValue("padding-left"); 
		_input.style.paddingTop = _spanStyle.getPropertyValue("padding-top"); 
		_input.style.paddingRight = _spanStyle.getPropertyValue("padding-right"); 
		_input.style.paddingBottom = _spanStyle.getPropertyValue("padding-bottom"); 
		textbox.setOwner(filterManager);
		if(sameAsEdit)
		{
			if(gridEXCell.getColumn().getEditType() != -1)
				textbox.setMaxLength(gridEXCell.getColumn().getMaxLength()); 
		}		
		textbox.Show();
		resetCommand.Show(_left+_width+1,_top,_height); 
		textbox.Focus(); 		
	}	
	var filterManager = this; 	
	return this; 
}
function GridEXFilterTextBoxManager(cell, sameAsEdit)
{
	var gridEXCell = cell; 
	var actAsEditTextBox = sameAsEdit;
	var resetCommand = null; 
	var textbox = gridEXCell.getGridEX().getEditControl(2);
	this.ApplyingInputMask = ApplyingInputMask; 
	this.KeyDown = KeyDown;
	this.KeyUp = KeyUp; 
	this.Leaving = Leaving;
	this.ResetFilter = ResetFilter; 
	this.TextChanged = TextChanged;
	this.Hide = Hide; 
	this.Show = Show; 	
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
		else if(window.event.keyCode == 13)
			EnterKeyDown(); 
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
	}
	function Leaving()
	{
		Hide(); 
	}
	function ApplyingInputMask()
	{
		return gridEXCell.getGridEX().FireEvent("ApplyingInputMask", [gridEXCell]);
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
				
			var _innerCellSpan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]);
			if(args != null)
				_innerCellSpan.innerText = args.getValue(); 
			else
				_innerCellSpan.innerText = newText;
				
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()
	{		
		if(gridEXCell.getValue() != null)
		{			
			gridEXCell.setValue(null);
			var _innerCellSpan = getInnerSpan(gridEXCell.getInnerCell().childNodes[0]);
			_innerCellSpan.innerText = "";
			gridEXCell.ResumeFilter(); 
			Hide(); 
		}
	}
	function EnterKeyDown()
	{
		Hide(); 
		gridEXCell.ResumeFilter(); 
	}	
	function EscKeyDown()
	{
		Hide(); 
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
	function Hide()
	{
		textbox.Hide(); 
		if(resetCommand != null)
			resetCommand.Hide(); 
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _innerCell = gridEXCell.getInnerCell();		
		var _innerCellSpan = getInnerSpan(_innerCell.childNodes[0]); 
		var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerCellSpan);		
		_left -= getOffsetLeftForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCellSpan);	
		_top -= getOffsetTopForEdit(gridEXCell.getGridEX().getHtmlGridEX()); 			
		var _width = _innerCell.offsetWidth - getPaddingRight(_innerCellSpan) - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.offsetHeight - getPaddingBottom(_innerCellSpan); 
		originalValue = gridEXCell.getValue(); 
		textbox.setLeft(_left); 
		textbox.setTop(_top); 		
		textbox.setWidth(_width); 
		textbox.setHeight(_height); 
		textbox.setStyle(document.defaultView.getComputedStyle(_innerCell, null)); 
		textbox.setText(gridEXCell.getValue()); 		
		var _input = textbox.getInnerHTML(); 		
		var _spanStyle = document.defaultView.getComputedStyle(_innerCellSpan, null); 
		_input.style.paddingLeft = _spanStyle.getPropertyValue("padding-left"); 
		_input.style.paddingTop = _spanStyle.getPropertyValue("padding-top"); 
		_input.style.paddingRight = _spanStyle.getPropertyValue("padding-right"); 
		_input.style.paddingBottom = _spanStyle.getPropertyValue("padding-bottom"); 
		textbox.setOwner(filterManager);
		if(sameAsEdit)
		{
			if(gridEXCell.getColumn().getEditType() != -1)
				textbox.setMaxLength(gridEXCell.getColumn().getMaxLength()); 
		}
		textbox.setInputMask(gridEXCell.getColumn().getInputMask());
		textbox.Show();
		resetCommand.Show(_left+_width+1,_top,_height); 
		textbox.Focus(); 		
	}	
	var filterManager = this; 	
	return this; 
}
