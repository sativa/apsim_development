//////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////////////
function getEditBox(id)
{
	var editBox = null; 
	editBox = eval("editBox = " + id + ";"); 
	if(editBox == null)
		throw new Error("invalid editBox with id '" + id + "'"); 		
		
	return editBox; 
}
function maskededitbox_onkeypress(id)
{
	var maskedEditBox = getEditBox(id); 
	maskedEditBox.onkeypress(); 
}
function maskededitbox_onkeydown(id)
{
	var maskedEditBox = getEditBox(id); 
	maskedEditBox.onkeydown(); 
}
function maskededitbox_onfocus(id)
{
	var maskedEditBox = getEditBox(id); 
	maskedEditBox.onfocus(); 
}
function maskededitbox_onblur(id)
{
	var maskedEditBox = getEditBox(id); 
	maskedEditBox.onblur(); 
}
function numericeditbox_onblur(id)
{
	var numericEditBox = getEditBox(id); 
	numericEditBox.onblur(); 
}
function numericeditbox_onfocus(id)
{
	var numericEditBox = getEditBox(id); 
	numericEditBox.onfocus(); 
}
function numericeditbox_onkeypress(id)
{
	var numericEditBox = getEditBox(id);
	numericEditBox.onkeypress(); 
}
function numericeditbox_onkeydown(id)
{
	var numericEditBox = getEditBox(id); 
	numericEditBox.onkeydown(); 
}
function numericeditbox_onkeyup(id)
{
	var numericEditBox = getEditBox(id); 
	numericEditBox.onkeyup(); 
}
function buttonup_onmousedown(id)
{
	var integerUpDown = getEditBox(id); 
	integerUpDown.onbuttonup(); 
}
function buttondown_onmousedown(id)
{
	var integerUpDown = getEditBox(id); 
	integerUpDown.onbuttondown(); 
}
function integerupdown_onmouseover(id,type)
{
	var integerUpDown = getEditBox(id); 
	integerUpDown.onbuttonmouseover(type); 
}
function integerupdown_onlateinitialize(id)
{
	var integerUpDown = getEditBox(id); 
	integerUpDown.onlateinitialize(); 
}