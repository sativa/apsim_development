//////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////////////
function getComboBox(id)
{
	var combobox =  null;
	combobox = eval("combobox = " + id + ";"); 
	if(combobox == null)
		throw new Error("invalid combobox with id '" + id + "'");
		
	return combobox;
}
function showComboBoxDropDown(id, element)
{
	var combobox = getComboBox(id); 
	combobox.showDropDown(element, getPixelLeft(element.offsetParent), getPixelTop(element)); 
}
function combobox_onmouseoverbutton(element, className)
{
	element.className = className; 
}
function combobox_onclick(id, element)
{
	var combobox = getComboBox(id); 
	combobox.onclick(element); 
}
function combobox_onkeydown(id)
{
	var combobox = getComboBox(id);
	combobox.onkeydown(); 
}
function combobox_onmouseover(id, element)
{
	var combobox = getComboBox(id);
	combobox.onmouseover(element); 
}
function combobox_onmouseout(id, element)
{
	var combobox = getComboBox(id); 
	combobox.onmouseout(element); 
}
function combo_onkeypress(id)
{
	var combobox = getComboBox(id); 
	combobox.input_onkeypress(); 
}
function combo_onkeydown(id)
{
	var combobox = getComboBox(id); 
	combobox.input_onkeydown(); 
}
function comboBoxOnBlur(id)
{
	var combobox = getComboBox(id); 
	combobox.onblur(); 
}


