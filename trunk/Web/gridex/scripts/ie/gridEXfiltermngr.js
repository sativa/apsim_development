/////////////////////////////////////////////////////////////////////
// GridEX JavaScript Filter Edit Manager (1.1.1009)
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
			
		divFilter.attachEvent("onmousedown", resetFilter_onmousedown); 
	}
	function Hide()
	{		
		divFilter.style.visibility = "hidden"; 
		currentManager = null; 
	}
	function Show(left, top, height)
	{
		if(divFilter.parentElement != null && divFilter.parentElement.tagName != "BODY")
			document.body.appendChild(divFilter);
			
		divFilter.style.visibility = "visible"; 
		divFilter.style.pixelLeft = left; 
		divFilter.style.pixelTop = top;
		divFilter.style.pixelHeight = height;
	}
	function resetFilter_onmousedown()
	{		
		if(currentManager != null)
			currentManager.ResetFilter(); 
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
		var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		var _innerCell = gridEXCell.getInnerCell(); 
		var _left = getPixelLeft(_innerCell);
		if(_rtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCell.childNodes[0]);
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1;
		var _height = _innerCell.childNodes[0].clientHeight;		
		if(_rtl)
			resetCommand.Show(_left, _top, _height); 
		else
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
	checkbox.attachEvent("onkeydown", filter_onkeydown); 
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
			var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
			if(cancel == null || !cancel)
			{
				if(combo.getInnerCombo().getAttribute("fdd") == "1" || combo.getInnerCombo().getAttribute("lar") != gridEXCell.getRow().getID())
				{					
					var input = document.getElementById(gridEXCell.getGridEX().getID() + "_eventdata");
					input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();				
					gridEXCell.getGridEX().DoPostBack(null, "DropDown", true);
					return false;					
				}
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
		gridEXCell.getInnerCell().setAttribute("niv", null);
		Hide();
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
	function ArrowLeftKeyDown()
	{
		if(window.event.srcElement != null && (window.event.srcElement == combo.getInnerTextBox() || window.event.srcElement.contains(combo.getInnerTextBox())))
			return; 
			
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getRow().PreviousFocusCell();
	}
	function ArrowDownKeyDown()
	{
		combo.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowUpKeyDown()
	{
		combo.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function ArrowRightKeyDown()
	{
		if(window.event.srcElement != null && (window.event.srcElement == combo.getInnerTextBox() || window.event.srcElement.contains(combo.getInnerTextBox())))
			return; 
			
		combo.Hide();
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
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
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
		if(resetCommand.getManager() == filterManager)
			resetCommand.Hide(); 
	}
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager);	
		var _rtl = 	(gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		var _innerCell = gridEXCell.getInnerCell(); 
		var _left = getPixelLeft(_innerCell) + 1;
		if(_rtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}		
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCell.childNodes[0]);
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1;
		var _height = _innerCell.childNodes[0].clientHeight;
		combo.setOwner(filterManager); 			
		if(_rtl)
			combo.setLeft(_left + resetCommand.getInnerHtml().offsetWidth);
		else
			combo.setLeft(_left);
		combo.setTop(_top); 
		combo.setWidth(_width); 
		combo.setHeight(_height);
		combo.setStyle(_innerCell.currentStyle);
		combo.setCharacterCasing(gridEXCell.getColumn().characterCasing); 
		var _input = combo.getInnerTextBox(); 
		_input.style.padding = _innerCell.childNodes[0].currentStyle.padding; 
		combo.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
		combo.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1)); 
		originalValue = gridEXCell.getValue(); 
		combo.setValue(gridEXCell.getValue());
		if(_rtl)
			resetCommand.Show(_left, _top, _height);
		else
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
			gridEXCell.setValue(null); 
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
		var _element = _innerCell.childNodes[0]; 
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
		
	//if(actAsEdit != null && actAsEdit == true)	
	valueList.setCompareTarget(1); 
	//else		
	//	valueList.setCompareTarget(2);  
		
	this.getCell = getCell; 		
	this.DropDown = DropDown; 
	this.EnterKeyDown = EnterKeyDown; 
	this.EscKeyDown = EscKeyDown; 
	this.KeyDown = KeyDown; 
	this.Leaving = Leaving; 
	this.ValueChanged = ValueChanged;
	this.ResetFilter = ResetFilter; 
	this.Show = Show; 	
	function getCell()
	{
		return gridEXCell;
	}	
	function Hide()
	{
		valueList.Hide(); 
		if(resetCommand.getManager() == filterManager)
			resetCommand.Hide(); 
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 		
		var _innerCell = gridEXCell.getInnerCell(); 
		var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		var _left = getPixelLeft(_innerCell);
		if(_rtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCell.childNodes[0]);				
		var _width = _innerCell.offsetWidth - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.childNodes[0].clientHeight;
		originalValue = gridEXCell.getValue(); 
		valueList.setOwner(filterManager);
		if(_rtl)
			valueList.setLeft(_left + resetCommand.getInnerHtml().offsetWidth); 
		else 			
			valueList.setLeft(_left);
		valueList.setTop(_top); 
		valueList.setWidth(_width); 
		valueList.setHeight(_height);	
		valueList.setItemCSS(gridEXCell.getRow().getTable().getRowCss(0)); 
		valueList.setSelectedItemCSS(gridEXCell.getRow().getTable().getRowCss(1)); 
		valueList.Show();
		if(_rtl)
			resetCommand.Show(_left, _top, _height); 
		else
			resetCommand.Show(_left+_width + 1, _top, _height); 
		valueList.Focus(); 		
	}
	function updateInnerCell(display, image)
	{	
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0]; 
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
	function ArrowLeftKeyDown()
	{
		valueList.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getRow().PreviousFocusCell();
	}
	function ArrowDownKeyDown()
	{
		valueList.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowUpKeyDown()
	{
		valueList.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function ArrowRightKeyDown()
	{		
		valueList.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getRow().NextFocusCell(); 
	}
	function DropDown()
	{
		var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
		if(cancel == null || !cancel)
		{
			if(valueList.ddpb)
			{
				if(valueList.getInnerList().getAttribute("fdd") == "1" || valueList.getInnerList().getAttribute("lar") != gridEXCell.getRow().getID())
				{					
					var input = document.getElementById(gridEXCell.getGridEX().getID() + "_eventdata");
					input.value = gridEXCell.getRow().getTable().getID() + ":" + gridEXCell.getColumn().getClientID();
					gridEXCell.getGridEX().DoPostBack(null, "DropDown", true);
					return false;		
				}
			}
		}
		valueList.setValue(gridEXCell.getValue()); 
	}
	function KeyDown()
	{
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
		var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
		if(cancel == null || !cancel)
		{
			if(gridEXCell.getGridEX().ddpb)
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
	function ArrowUpKeyDown()
	{
		dropdown.Hide(); 
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function ArrowLeftKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getRow().PreviousFocusCell();
	}
	function ArrowDownKeyDown()
	{
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowRightKeyDown()
	{		
		dropdown.Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getRow().NextFocusCell(); 
	}
	function KeyDown()
	{
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
	}
	function EnterKeyDown()
	{
		gridEXCell.ResumeFilter(); 
	}
	function EscKeyDown()
	{
		Hide(); 		
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
		var _element = _innerCell.childNodes[0]; 
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
		if(resetCommand.getManager() == filterManager)
			resetCommand.Hide();
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1"); 
		var _innerCell = gridEXCell.getInnerCell(); 
		var _left = getPixelLeft(_innerCell.childNodes[0]);
		if(_rtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}		
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCell.childNodes[0]);		
		var _width = _innerCell.clientWidth - resetCommand.getInnerHtml().offsetWidth; 
		var _height = _innerCell.offsetHeight;		
		originalValue = gridEXCell.getValue();
		if(_rtl)
			dropdown.setLeft(_left + resetCommand.getInnerHtml().offsetWidth); 
		else
			dropdown.setLeft(_left); 
		dropdown.setTop(_top); 
		dropdown.setWidth(_width); 
		dropdown.setHeight(_height); 			
		dropdown.setOwner(filterManager);
		dropdown.setValue(gridEXCell.getValue());
		if(_rtl)
			resetCommand.Show(_left, _top, _height); 
		else
			resetCommand.Show(_left+_width, _top, _height); 
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
	this.NotInList = NotInList; 	
	this.Leaving = Leaving; 
	this.ValueChanged = ValueChanged; 
	this.ResetFilter = ResetFilter; 
	this.Show = Show;		
	function getCell()
	{
		return gridEXCell; 
	}
	function ArrowLeftKeyDown()
	{
		if(window.event.srcElement != null && (window.event.srcElement == dropdown.getInnerTextBox() || window.event.srcElement.contains(dropdown.getInnerTextBox())))
			return; 
			
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getRow().PreviousFocusCell(); 
	}
	function ArrowRightKeyDown()
	{
		if(window.event.srcElement != null && (window.event.srcElement == dropdown.getInnerTextBox() || window.event.srcElement.contains(dropdown.getInnerTextBox())))
			return; 
			
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive(); 
			
		gridEXCell.getRow().NextFocusCell(); 
	}
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
	function DropDown()
	{
		var cancel = gridEXCell.getGridEX().FireEvent("DropDown", [gridEXCell]);
		if(cancel == null || !cancel)	
		{
			if(gridEXCell.getGridEX().ddpb)
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
	function Leaving()
	{
		if(resetCommand != null && resetCommand.getManager() == filterManager)
		{
			if(document.activeElement == resetCommand.getInnerHtml() || resetCommand.getInnerHtml().contains(document.activeElement))
				return true;
		}
		Hide(); 
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
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown();
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown();
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
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
			
			gridEXCell.getInnerCell().setAttribute("niv", null); 
			updateInnerCell(dropdown.getDisplay(), originalImage);
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
		var _element = _innerCell.childNodes[0]; 
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
		resetCommand.Hide(); 
	}
	function EnterKeyDown()
	{	
		dropdown.Hide(); 
		gridEXCell.ResumeFilter(); 
	}	
	function EscKeyDown()
	{		
		gridEXCell.getInnerCell().setAttribute("niv", null);
		Hide(); 
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
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager);
		var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		var _innerCell = gridEXCell.getInnerCell(); 
		var _left = getPixelLeft(_innerCell.childNodes[0]);
		if(_rtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCell.childNodes[0]);
		var _width = _innerCell.clientWidth - resetCommand.getInnerHtml().offsetWidth; 
		var _height = _innerCell.offsetHeight;
		originalValue = gridEXCell.getValue(); 
		if(_rtl)
			dropdown.setLeft(_left+resetCommand.getInnerHtml().offsetWidth);
		else
			dropdown.setLeft(_left); 
		dropdown.setTop(_top); 
		dropdown.setWidth(_width); 
		dropdown.setHeight(_height); 
		dropdown.setStyle(_innerCell.currentStyle);
		dropdown.setOwner(filterManager);
		dropdown.setCharacterCasing(gridEXCell.getColumn().characterCasing); 
		var _input = dropdown.getInnerTextBox(); 
		_input.style.padding = _innerCell.childNodes[0].currentStyle.padding;
		if(gridEXCell.getInnerCell().getAttribute("niv") != null)
			dropdown.setValue(null, gridEXCell.getInnerCell().getAttribute("niv")); 
		else
			dropdown.setValue(gridEXCell.getValue());
		if(_rtl)
			resetCommand.Show(_left, _top, _height); 
		else
			resetCommand.Show(_left+_width, _top, _height); 
		dropdown.Show();		
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
	this.Leaving = Leaving; 
	this.ResetFilter = ResetFilter; 
	this.ValueChanged = ValueChanged; 	
	this.Show = Show; 
	function ArrowLeftKeyDown()
	{
		if(window.event.srcElement != null && (window.event.srcElement == calendar.getInnerTextBox() || window.event.srcElement.contains(calendar.getInnerTextBox())))
			return; 
			
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getRow().PreviousFocusCell();		
	}
	function ArrowUpKeyDown()
	{
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();		
			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function ArrowDownKeyDown()
	{		
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getGridEX().MoveNext(); 
	}
	function ArrowRightKeyDown()
	{
		if(window.event.srcElement != null && (window.event.srcElement == calendar.getInnerTextBox() || window.event.srcElement.contains(calendar.getInnerTextBox())))
			return; 
			
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getRow().NextFocusCell(); 
	}
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
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown(); 
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
	}
	function Leaving()
	{
		Hide();
	}
	function updateInnerCell(display)
	{
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0];
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
		if(resetCommand.getManager() == filterManager)
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
		var _isrtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		var _innerCell = gridEXCell.getInnerCell(); 
		var _left = getPixelLeft(_innerCell.childNodes[0]);
		if(_isrtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}		
		var _top = getPixelTop(_innerCell) + getBorderTopWidth(_innerCell); 
		var _width = _innerCell.clientWidth - resetCommand.getInnerHtml().offsetWidth; 
		var _height = _innerCell.offsetHeight; 
		originalValue = gridEXCell.getValue(); 
		if(_isrtl)
			calendar.setLeft(_left + resetCommand.getInnerHtml().offsetWidth); 
		else
			calendar.setLeft(_left); 
		calendar.setTop(_top); 
		calendar.setWidth(_width); 
		calendar.setHeight(_height);			
		calendar.setStyle(_innerCell.currentStyle); 
		calendar.setOwner(filterManager);
		calendar.setSelectedDate(gridEXCell.getValue());
		var _input = calendar.getInnerTextBox(); 
		_input.style.padding = _innerCell.childNodes[0].currentStyle.padding;
		if(_isrtl)
			resetCommand.Show(_left, _top, _height); 
		else
			resetCommand.Show(_left+_width,_top,_height); 
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
	var filterList = null;
	this.getCell = getCell;
	this.KeyDown = KeyDown; 	
	this.Leaving = Leaving; 	
	this.ResetFilter = ResetFilter; 
	this.ValueChanged = ValueChanged; 		
	this.Show = Show;
	this.Hide = Hide;
	function ArrowUpKeyDown()
	{
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getGridEX().MovePrevious(); 
	}
	function ArrowDownKeyDown()
	{
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getGridEX().MoveNext();
	}
	function ArrowLeftKeyDown()
	{
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getRow().PreviousFocusCell(); 
	}
	function ArrowRightKeyDown()
	{		
		Hide();
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
			
		gridEXCell.getRow().NextFocusCell(); 
	}
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
		else if(window.event.keyCode == 37)
			ArrowLeftKeyDown();
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown();
		else if(window.event.keyCode == 39)
			ArrowRightKeyDown();
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 9)
			TabKeyDown(); 
	}		
	function setConditionOperator(value)
	{		
		gridEXCell.setFilterConditionOperator(value); 
	}
	function updateInnerCell(display)
	{
		var _innerCell = gridEXCell.getInnerCell(); 
		var _element = _innerCell.childNodes[0]; 				
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
	function EnterKeyDown()
	{		
		gridEXCell.ResumeFilter(); 
	}	
	function EscKeyDown()
	{
		Hide();  
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
	function Hide()
	{
		calendar.Hide(); 
		if(resetCommand.getManager() == filterManager)
			resetCommand.Hide(); 
		if(filterList != null)
			filterList.Hide(); 
	}	
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 				
		var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		var _innerCell = gridEXCell.getInnerCell(); 		
		var _left = getPixelLeft(_innerCell.childNodes[0]);
		if(_rtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}		
		var _top = getPixelTop(_innerCell) + getBorderTopWidth(_innerCell);
		var _width = _innerCell.clientWidth;
		_width -= (resetCommand.getInnerHtml().offsetWidth); 
		var _height = _innerCell.offsetHeight;	
		originalValue = gridEXCell.getValue();
		if(_rtl)
			calendar.setLeft(_left+resetCommand.getInnerHtml().offsetWidth);
		else
			calendar.setLeft(_left); 
		calendar.setTop(_top); 
		calendar.setWidth(_width); 
		calendar.setHeight(_height);			
		calendar.setOwner(filterManager);		
		calendar.setSelectedDate(null);		
		if(_rtl)
			resetCommand.Show(_left+_width,_top,_innerCell.offsetHeight); 
		else
			resetCommand.Show(_left+_width,_top,_height);
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
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 	
	}
	function KeyUp()
	{
		gridEXCell.getGridEX().FireEvent("EditingKeyUp", [gridEXCell, window.event.keyCode]); 
	}	
	function Leaving()
	{
		if(document.activeElement == resetCommand.getInnerHtml() || resetCommand.getInnerHtml().contains(document.activeElement))
			return;
			
		if(document.activeElement == textbox.getInnerHTML() || textbox.getInnerHTML().contains(document.activeElement))
			return;
			
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
			var requiresNOBR = (gridEXCell.getInnerCell().childNodes[0].getElementsByTagName("NOBR").length == 1);
			if(!requiresNOBR)		
				gridEXCell.getInnerCell().childNodes[0].innerText = newText;
			else
				gridEXCell.getInnerCell().childNodes[0].innerHTML = "<NOBR>" + newText + "</NOBR>"; 
				
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()
	{
		if(gridEXCell.getValue() != null)
		{					
			gridEXCell.setValue(null);
			var requiresNOBR = (gridEXCell.getInnerCell().childNodes[0].getElementsByTagName("NOBR").length == 1);
			if(!requiresNOBR)
				gridEXCell.getInnerCell().childNodes[0].innerText = ""; 	
			else			
				gridEXCell.getInnerCell().childNodes[0].innerHTML = "<NOBR></NOBR>"; 
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
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false;
	}
	function ApplyingInputMask()
	{
		return gridEXCell.getGridEX().FireEvent("ApplyingInputMask", [gridEXCell]); 
	}
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
	function Hide()
	{
		textbox.Hide(); 
		if(resetCommand.getManager() == filterManager)
			resetCommand.Hide(); 
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _rtl  = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1");
		var _innerCell = gridEXCell.getInnerCell();			
		var _left = getPixelLeft(_innerCell.childNodes[0]);		
		if(_rtl)
		{				
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}		
		var _top = getPixelTop(_innerCell) + getBorderTopWidth(_innerCell); 
		var _width = _innerCell.clientWidth - resetCommand.getInnerHtml().offsetWidth; 		
		var _height = _innerCell.offsetHeight;
		textbox.setCharacterCasing(gridEXCell.getColumn().characterCasing);
		textbox.setInputMask(gridEXCell.getColumn().getInputMask());
		textbox.setInvalidValueAction(gridEXCell.getColumn().getInvalidValueAction()); 
		textbox.setInvalidValueMessage(gridEXCell.getColumn().getInvalidValueMessage());
		if(_rtl)
			textbox.setLeft(_left + resetCommand.getInnerHtml().offsetWidth); 
		else
			textbox.setLeft(_left); 
		textbox.setTop(_top); 
		textbox.setWidth(_width); 
		textbox.setHeight(_height); 
		textbox.setStyle(_innerCell.currentStyle); 
		originalValue = gridEXCell.getValue(); 
		textbox.setText(gridEXCell.getValue()); 		
		var _input = textbox.getInnerHTML(); 
		_input.style.padding = _innerCell.childNodes[0].currentStyle.padding; 		
		textbox.setOwner(filterManager);
		if(sameAsEdit)
		{
			if(gridEXCell.getColumn().getEditType() != -1)
				textbox.setMaxLength(gridEXCell.getColumn().getMaxLength()); 
		}	
		if(_rtl)
			resetCommand.Show(_left, _top, _height); 
		else			
			resetCommand.Show(_left+_width,_top, _height);  	
		textbox.Show();				
		textbox.Focus(); 		
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
		else if(window.event.keyCode == 40)
			ArrowDownKeyDown(); 
		else if(window.event.keyCode == 38)
			ArrowUpKeyDown(); 
	}	
	function Leaving()
	{
		if(document.activeElement == resetCommand.getInnerHtml() || resetCommand.getInnerHtml().contains(document.activeElement))
			return;
			
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
				
			var requiresNOBR = (gridEXCell.getInnerCell().childNodes[0].getElementsByTagName("NOBR").length == 1);
			if(!requiresNOBR)		
				gridEXCell.getInnerCell().childNodes[0].innerText = PasswordText(newText, gridEXCell.getColumn().getPasswordChar());
			else
				gridEXCell.getInnerCell().childNodes[0].innerHTML = "<NOBR>" + PasswordText(newText, gridEXCell.getColumn().getPasswordChar()) + "</NOBR>"; 
				
			gridEXCell.getInnerCell().setAttribute("text", newText);
			gridEXCell.getGridEX().FireEvent("CellUpdated", [gridEXCell]);
		}
	}
	function ResetFilter()
	{
		if(gridEXCell.getValue() != null)
		{					
			gridEXCell.setValue(null);
			var requiresNOBR = (gridEXCell.getInnerCell().childNodes[0].getElementsByTagName("NOBR").length == 1);
			if(!requiresNOBR)
				gridEXCell.getInnerCell().childNodes[0].innerText = ""; 	
			else			
				gridEXCell.getInnerCell().childNodes[0].innerHTML = "<NOBR></NOBR>"; 
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
		if(gridEXCell.getGridEX().getHtmlGridEX().setActive != null)
			gridEXCell.getGridEX().getHtmlGridEX().setActive();
		window.event.returnValue = false;
		window.event.cancelBubble = true;		
		return false;
	}
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
	function Hide()
	{
		textbox.Hide(); 
		if(resetCommand.getManager() == filterManager)
			resetCommand.Hide(); 
	}
	var originalValue = null; 
	function Show()
	{
		resetCommand = gridEXCell.getGridEX().getResetFilterCommand(); 
		resetCommand.setManager(filterManager); 
		var _rtl = (gridEXCell.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1"); 
		var _innerCell = gridEXCell.getInnerCell();			
		var _left = getPixelLeft(_innerCell) + getPaddingLeft(_innerCell.childNodes[0]);		
		if(_rtl)
		{			
			_left += (gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft;
			_left -= ((gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - gridEXCell.getGridEX().getRootTable().getHtmlItemsTable().offsetParent.scrollLeft);
		}		
		var _top = getPixelTop(_innerCell) + getPaddingTop(_innerCell.childNodes[0]);				
		var _width = _innerCell.clientWidth - getPaddingRight(_innerCell.childNodes[0]) - resetCommand.getInnerHtml().offsetWidth - 1; 
		var _height = _innerCell.clientHeight - getPaddingBottom(_innerCell.childNodes[0]);
		if(_rtl)
			textbox.setLeft(_left+resetCommand.getInnerHtml().offsetWidth);
		else
			textbox.setLeft(_left); 
		textbox.setTop(_top); 
		textbox.setWidth(_width); 
		textbox.setHeight(_height); 
		textbox.setStyle(_innerCell.currentStyle); 
		textbox.setText(gridEXCell.getValue()); 		
		var _input = textbox.getInnerHTML(); 
		_input.style.padding = _innerCell.childNodes[0].currentStyle.padding; 		
		textbox.setOwner(filterManager);
		if(sameAsEdit)
		{
			if(gridEXCell.getColumn().getEditType() != -1)
				textbox.setMaxLength(gridEXCell.getColumn().getMaxLength()); 
		}
		originalValue = gridEXCell.getValue();
		if(_rtl)
			resetCommand.Show(_left, _top, _height); 
		else
			resetCommand.Show(_left+_width+1,_top, _height);		
		textbox.Show();		
		textbox.Focus(); 		
	}	
	var filterManager = this; 	
	return this; 
}