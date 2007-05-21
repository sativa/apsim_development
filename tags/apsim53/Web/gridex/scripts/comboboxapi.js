//////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////////////
function ComboBox(id, properties, cssItems, events)
{
	var dropdown = null;
	var id = id; 
	var cssItems = cssItems;
	var events = events;
	var selectedValue = properties[0]; 
	var selectedIndex = properties[1]; 
	var svpb = (properties[2] == 1) ? true :  false; 
	var sipb = (properties[3] == 1) ? true : false; 
	var casing = properties[4]; 
	var autoPB = (properties[5] == 1); 
	this.SelectedValue = selectedValue; 
	this.SelectedIndex = selectedIndex; 
	this.onblur = onblur; 
	this.onclick = onclick;
	this.onkeydown = onkeydown;
	this.input_onkeydown = input_onkeydown;
	this.input_onkeypress = input_onkeypress;	
	this.onmouseout = onmouseout; 
	this.onmouseover = onmouseover;
	this.showDropDown = showDropDown;	 
	function updateStatus()
	{		
		var element = document.getElementsByName(id+"_status"); 
		if(element != null && element.length > 0)
		{
			var status = "";
			status = comboBox.SelectedIndex;
			element[0].value = status; 			
		}
	}
	function doPostBack(eventArgument)
	{
		updateStatus();
		__doPostBack(document.getElementById(id).getAttribute("name"), eventArgument); 
	}
	function isDropDownVisible()
	{
		if(dropdown == null || dropdown.style.visibility != "visible")
			return false; 			
		return true; 
	}
	function hideDropDown()
	{
		if(dropdown != null)
			dropdown.style.visibility = "hidden";			
		hideBackFrame("jweBackFrame"); 
		dropdown = null; 
	}
	function showDropDownItem(index)
	{
		var table = dropdown.getElementsByTagName("TABLE")[0]; 
		if(table.rows.length == 0)
			return; 			
		var item = null;
		if(index == -1)
			item = table.rows[0]; 
		else
			item = table.rows[index];			
		selectCell(item.cells[1]); 
	}
	var button = null; 
	function showDropDown(element, x, y)
	{
		if(isDropDownVisible())
		{
			hideDropDown();
			return; 
		}
		button = element; 
		dropdown = document.getElementById(id+"_items");
		if(dropdown.parentElement != null && dropdown.parentElement.tagName != "BODY")
			document.body.appendChild(dropdown); 
		dropdown.style.left = x + "px"; 
		dropdown.style.top = (y + element.offsetHeight) + "px";
		dropdown.style.width = element.offsetParent.offsetWidth + "px"; 
		dropdown.style.visibility = "visible"; 
		showBackFrame("jweBackFrame",dropdown.style.pixelLeft,dropdown.style.pixelTop,dropdown.offsetWidth,dropdown.offsetHeight); 
		if(dropdown.focus != null)
			dropdown.focus();
		else if(dropdown.onfocus != null)
			dropdown.onfocus();		
		showDropDownItem(comboBox.SelectedIndex); 
		if(browser.isNetscape)
			document.activeElement = dropdown; 
	}
	var selectedCell = null;
	function unSelectCell(cell)
	{
		if(cell != null)
		{
			var css = cell.className; 
			var i = css.indexOf(cssItems[1]);
			if(i > 0)
			{
				css = css.substring(0, i-1); 
				cell.className = css; 
			}
		}
	}
	function selectCell(cell)
	{
		unSelectCell(selectedCell);
		cell.className += " " + cssItems[1]; 
		selectedCell = cell; 
	}
	function updateEditor(cell)
	{
		var element = document.getElementById(id + "_editor"); 
		element.value = trim(cell.innerText); 			
		var img = cell.getElementsByTagName("IMG");
		if(img != null && img.length > 0)
		{
			element.style.backgroundImage = "url(" + img[0].src + ")"; 
			element.style.backgroundRepeat = "no-repeat"; 
			element.style.backgroundPosition = "left center"; 
			element.style.paddingLeft = "18px"; 
		}
		else
		{
			element.style.backgroundImage = "url(none)";
			element.style.paddingLeft = "0px"; 
		}
	}
	function selectCellCore(cell,refreshEditor)
	{
		var tr = cell.parentElement;
		var value = tr.cells[0].innerHTML;
		var index = tr.rowIndex; 
		var csi = false; 
		var csv = false; 
		if(index != comboBox.SelectedIndex)
		{
			comboBox.SelectedIndex = index; 
			fireEvent("SelectedIndexChanged", [comboBox]); 			
			csi = true; 
		}
		if(value != comboBox.SelectedValue)
		{
			comboBox.SelectedValue = value; 
			fireEvent("SelectedValueChanged", [comboBox]);			
			csv = true; 
		}
		if(csi || csv)
		{
			if(refreshEditor == null || refreshEditor == true)
				updateEditor(cell);
		}
		selectCell(cell); 		
		hideDropDown();		
		updateStatus(); 
		if(csi && sipb)
			doPostBack("SelectedIndexChanged"); 
		else if((csv && svpb) || autoPB)			
			doPostBack("SelectedValueChanged"); 		
	}
	function onclick(cell)
	{
		if(button == null)
			button = cell; 
			
		selectCellCore(cell);
	}	
	function onkeydown()
	{
		var table = null; 
		var i = -1;
		if(window.event.keyCode == 13)
			selectCellCore(selectedCell); 
		else if(window.event.keyCode == 27)
			hideDropDown(); 
		else if(window.event.keyCode == 33)
		{
			if(selectedCell != null)
				i = 0;
		}
		else if(window.event.keyCode == 34)
		{
			if(selectedCell != null)
			{
				table = dropdown.getElementsByTagName("TABLE")[0]; 
				i = table.rows.length - 1;	
			}
		}
		else if(window.event.keyCode == 38)
		{
			if(selectedCell != null)
			{
				i = selectedCell.parentElement.rowIndex;
				if( i - 1 >= 0)
					i = i -1; 				
			}
		}
		else if(window.event.keyCode == 40)
		{
			table = dropdown.getElementsByTagName("TABLE")[0]; 
			i = selectedCell.parentElement.rowIndex;
			if(i + 1< table.rows.length)
				i = i + 1;			
		}
		if(i != -1)
		{
			if(table == null)
				table = dropdown.getElementsByTagName("TABLE")[0]; 
			cell = table.rows[i].cells[1]; 
			selectCell(cell);
		}
	}
	function input_onkeydown()
	{		
		var i = -1;
		var table = null;
		if(window.event.keyCode == 33)
		{
			i = 0;
			window.event.cancelBubble = true;
			window.event.returnValue = false;
		}
		else if(window.event.keyCode == 34)
		{
			if(dropdown == null)
				dropdown = document.getElementById(id+"_items");
				
			table = dropdown.getElementsByTagName("TABLE")[0];
			i = table.rows.length - 1; 
			window.event.cancelBubble = true;
			window.event.returnValue = false; 
		}
		else if(window.event.keyCode == 38)
		{
			if(selectedCell != null)
			{
				i = selectedCell.parentElement.rowIndex;
				if(i - 1 >= 0)
					i = i -1; 
				else
					i = -1;
			}
			if(i == -1)
			{
				if(dropdown == null)
					dropdown = document.getElementById(id+"_items");					
				table = dropdown.getElementsByTagName("TABLE")[0]; 
				i = table.rows.length - 1; 
			}
		}
		else if(window.event.keyCode == 40)
		{
			if(dropdown == null)
				dropdown = document.getElementById(id+"_items");				
			table = dropdown.getElementsByTagName("TABLE")[0]; 
			if(selectedCell != null)
			{
				i = selectedCell.parentElement.rowIndex;
				if(i + 1 < table.rows.length)
					i = i + 1;
				else
					i = -1;
			}
			if(i == -1)
				i = 0; 
		}
		if(i != -1)
		{
			if(table == null)
			{
				if(dropdown == null)
					dropdown = document.getElementById(id+"_items");					
				table = dropdown.getElementsByTagName("TABLE")[0];
			}
			if(i >= 0 && i < table.rows.length)
			{
				cell = table.rows[i].cells[1];
				selectCellCore(cell); 
				var element = window.event.srcElement;
				if(element.select != null)
					element.select(); 
			}			
		}
	}
	function findCellWithText(text)
	{
		var table = null; 
		if(dropdown == null)
			dropdown = document.getElementById(id+"_items");			
		text = trim(text);
		table = dropdown.getElementsByTagName("TABLE")[0]; 
		for(var i=0;i<table.rows.length;i++)
		{
			var cell = table.rows[i].cells[1];
			var innerText = trim(cell.innerText);
			if(innerText.toUpperCase().indexOf(text.toUpperCase()) == 0)
				return cell;
		}
		return null; 
	}
	function input_onkeypress()
	{	
		if(browser.isNetscape)
			return; 			
		var c, C, newChar; 
		var textToFind, textDisplay, textRemain;
		var range, selectedRange; 
		if(casing == 1)
		{
			try
			{
				C = window.event.keyCode;
				newChar = String.fromCharCode(C).toLowerCase();
				c = newChar.charCodeAt();
				window.event.keyCode = c;
			}
			catch (err) { } 
		}
		else if(casing == 3)
		{
			try
			{
				c = window.event.keyCode;
				newChar = String.fromCharCode(c).toUpperCase();
				C = newChar.charCodeAt(); 			
				window.event.keyCode = C; 	
			} catch(err) { } 
		}
		else
			newChar = String.fromCharCode(window.event.keyCode); 
		
		var textbox = window.event.srcElement; 
		textToFind = textbox.value; 
		textDisplay = textbox.value; 		
		if(document.activeElement == textbox && document.selection != null && document.selection.type == "Text")
		{
			range = textbox.createTextRange();
			selectedRange = document.selection.createRange();
			if(selectedRange.compareEndPoints("EndToEnd", range) == 0 && selectedRange.compareEndPoints("StartToStart", range) != 0)
			{
				indexOf = textDisplay.lastIndexOf(selectedRange.text);
				if(indexOf > 0)
				{
					textToFind = textDisplay.substr(0, indexOf);
					textToFind += newChar; 
				}
			}
			else if(selectedRange.compareEndPoints("EndToEnd", range) == 0 && selectedRange.compareEndPoints("StartToStart", range) == 0)
				textToFind = newChar; 
			else
				textToFind += newChar; 
		}
		else
			textToFind += newChar;
			
		var cell = findCellWithText(textToFind);
		if(cell != null)
		{			
			updateEditor(cell); 
			var display = trim(cell.innerText);
			textRemain = display.substr(textToFind.length);
			selectedRange = textbox.createTextRange();	
			selectedRange.moveStart("character", textToFind.length);
			selectedRange.moveEnd("character", textRemain.length); 
			selectedRange.select(); 
			selectCellCore(cell, false);
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false; 
		}
		else
		{					
			var u = false; 
			if(comboBox.SelectedIndex != -1)
			{
				comboBox.SelectedIndex = -1;
				fireEvent("SelectedIndexChanged", [comboBox]); 
				u = true; 
			}
			if(comboBox.SelectedValue != null)
			{
				comboBox.SelectedValue = null;
				fireEvent("SelectedValueChanged", [comboBox]); 
				u = true; 
			}
			if(u)
				updateStatus(); 
		}
	}
	function onblur()
	{
		if(isDropDownVisible())
		{
			if(document.activeElement == dropdown || dropdown.contains(document.activeElement))
				return false;
				
			if(document.activeElement == button || button.contains(document.activeElement))
				return false;	
				
			hideDropDown(); 
		}
	}
	function onmouseover(element)
	{
		selectCell(element); 
	}
	function onmouseout(element)
	{
		if(dropdown != null && dropdown.contains(window.event.toElement))
		{					
			if(window.event.toElement != null && window.event.toElement == dropdown)
			{ } 
			else
			{
				unSelectCell(element); 
			}
		}
	}
	function fireEvent(name, params)
	{
		return fireEventEx(events, name, params); 		
	}
	var element = document.getElementById(id+"_items");		
	if(browser.isIE)
		element.attachEvent("onblur", function() { comboBoxOnBlur(id) });
		
	fireEvent("Load", null); 
	var comboBox = this; 
	return this;
}
