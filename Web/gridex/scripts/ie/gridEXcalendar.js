//////////////////////////////////////////////////////////////////
// GridEX JavaScript API  (1.1.1009)
// Copyright by Janus Systems S.A.
// 2002 - 2004
//////////////////////////////////////////////////////////////////
function showBackFrame(id, left, top, width, height)
{
	var element = document.getElementById(id);
	if(element == null)
	{
		var frameSrc = "javascript:void(0);";
		element = document.createElement("IFRAME");
		try
		{			
			frameSrc = editorsFrameUrl;
		} catch(err) { }
		element.src = frameSrc; 
		document.body.appendChild(element); 
		element.id = id;
		element.style.visibility = "hidden"; 
		element.style.position = "absolute";
		element.style.zIndex = 999; 		
	}   
	element.style.pixelLeft = left;
	element.style.pixelTop = top;
	element.style.pixelWidth = width;
	element.style.pixelHeight = height; 
	element.style.visibility = "visible"; 
}
function showCalendarBackFrame(left, top, width, height)
{	  
	showBackFrame("backCalendarFrame", left, top, width, height);	
}
function hideBackFrame(id)
{
	var element = document.getElementById(id);
	if(element != null)
		element.style.visibility = "hidden"; 
}
function hideCalendarBackFrame()
{
	hideBackFrame("backCalendarFrame"); 
}
function showCalendarBackMonthsFrame(left, top, width, height)
{	
	showBackFrame("backCalendarMonthsFrame", left, top, width, height);	
}
function hideCalendarBackMonthsFrame()
{
	hideBackFrame("backCalendarMonthsFrame"); 
}
// date format functions (based on http://www.mattkruse.com/) // 
function LZ(x) 
{
	return(x<0||x>9?"":"0")+x
}
function _isInteger(val) 
{
	var digits="1234567890";
	for (var i=0; i < val.length; i++) 
	{
		if (digits.indexOf(val.charAt(i))==-1)
			return false;
	}	
	return true;
}
function _getInt(str,i,minlength,maxlength) 
{
	var token = null; 
	for (var x=maxlength; x >= minlength; x--) 
	{
		token=str.substring(i,i+x);
		if (token.length < minlength)
			return null;
		if (_isInteger(token))
			return token; 
	}
	return null;
}
function isDate(val,format) 
{
	var date= getDateFromFormat(val,format);
	if(date==0)
		return false;
		
	return true;
}
function formatDate(date,format) 
{
	format=format+"";
	var result="";
	var i_format=0;
	var c="";
	var token="";
	var y=date.getYear()+"";
	var M=date.getMonth()+1;
	var d=date.getDate();
	var E=date.getDay();
	var H=date.getHours();
	var m=date.getMinutes();
	var s=date.getSeconds();
	var yyyy,yy,MMM,MM,dd,hh,h,mm,ss,ampm,HH,H,KK,K,kk,k;
	// Convert real date parts into formatted versions
	var value=new Object();
	if (y.length < 4) {y=""+(y-0+1900);}
	value["y"]=""+y;
	value["yyyy"]=y;
	value["yy"]=y.substring(2,4);
	value["M"]=M;
	value["MM"]=LZ(M);	
	value["d"]=d;
	value["dd"]=LZ(d);
	value["H"]=H;
	value["HH"]=LZ(H);
	if (H==0){value["h"]=12;}
	else if (H>12){value["h"]=H-12;}
	else {value["h"]=H;}
	value["hh"]=LZ(value["h"]);
	if (H>11){value["K"]=H-12;} else {value["K"]=H;}
	value["k"]=H+1;
	value["KK"]=LZ(value["K"]);
	value["kk"]=LZ(value["k"]);
	if (H > 11) { value["a"]="PM"; }
	else { value["a"]="AM"; }
	value["m"]=m;
	value["mm"]=LZ(m);
	value["s"]=s;
	value["ss"]=LZ(s);
	while (i_format < format.length) 
	{
		c=format.charAt(i_format);
		token="";
		while ((format.charAt(i_format)==c) && (i_format < format.length)) 
			token += format.charAt(i_format++);

		if (value[token] != null)
			result=result + value[token];
		else
			result=result + token;
	}
	return result;
}
function getDateFromFormat(val,format) 
{
	val=val+"";
	format=format+"";
	var i_val=0;
	var i_format=0;
	var c="";
	var token="";
	var token2="";
	var x,y;
	var now=new Date();
	var year=now.getYear();
	var month=now.getMonth()+1;
	var date=1;
	var hh=now.getHours();
	var mm=now.getMinutes();
	var ss=now.getSeconds();
	var ampm="";
	
	while (i_format < format.length) {
		// Get next token from format string
		c=format.charAt(i_format);
		token="";
		while ((format.charAt(i_format)==c) && (i_format < format.length)) {
			token += format.charAt(i_format++);
			}
		// Extract contents of value based on format token
		if (token=="yyyy" || token=="yy" || token=="y") {
			if (token=="yyyy") { x=4;y=4; }
			if (token=="yy")   { x=2;y=2; }
			if (token=="y")    { x=2;y=4; }
			year=_getInt(val,i_val,x,y);
			if (year==null) { return 0; }
			i_val += year.length;
			if (year.length==2) {
				if (year < 70) { year=1900+(year-0); }
				else { year=2000+(year-0); }
				}
			}		
		else if (token=="MM"||token=="M") {
			month=_getInt(val,i_val,token.length,2);
			if(month==null||(month<1)||(month>12)){return 0;}
			i_val+=month.length;}
		else if (token=="dd"||token=="d") {
			date=_getInt(val,i_val,token.length,2);
			if(date==null||(date<1)||(date>31)){return 0;}
			i_val+=date.length;}
		else if (token=="hh"||token=="h") {
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<1)||(hh>12)){return 0;}
			i_val+=hh.length;}
		else if (token=="HH"||token=="H") {
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<0)||(hh>23)){return 0;}
			i_val+=hh.length;}		
		else if (token=="mm"||token=="m") {
			mm=_getInt(val,i_val,token.length,2);
			if(mm==null||(mm<0)||(mm>59)){return 0;}
			i_val+=mm.length;}
		else if (token=="ss"||token=="s") {
			ss=_getInt(val,i_val,token.length,2);
			if(ss==null||(ss<0)||(ss>59)){return 0;}
			i_val+=ss.length;}
		else if (token=="a") {
			if (val.substring(i_val,i_val+2).toLowerCase()=="am") {ampm="AM";}
			else if (val.substring(i_val,i_val+2).toLowerCase()=="pm") {ampm="PM";}
			else {return 0;}
			i_val+=2;}
		else {
			if (val.substring(i_val,i_val+token.length)!=token) {return 0;}
			else {i_val+=token.length;}
			}
		}
	// If there are any trailing characters left in the value, it doesn't match
	if (i_val != val.length) { return 0; }
	// Is date valid for month?
	if (month==2) {
		// Check for leap year
		if ( ( (year%4==0) && (year%100 != 0) ) || (year%400==0) ) { // leap year
			if (date > 29){ return false; }
			}
		else { if (date > 28) { return false; } }
		}
	if ((month==4)||(month==6)||(month==9)||(month==11)) {
		if (date > 30) { return false; }
		}
	// Correct hours value
	if (hh<12 && ampm=="PM")
		hh=hh-0+12;
	else if (hh >11 && ampm=="AM")
		hh-=12;
	
	var newdate=new Date(year,month-1,date,hh,mm,ss);
	return newdate;
}
function gcctoday_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").today_onclick(); 
}
function gccnone_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").none_onclick(); 
}
function gccbutton_onblur()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement; 
	if(element != null)
		element.getAttribute("calendar").button_onblur(); 
}
function gccbutton_onmousedown()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").button_onmousedown(); 
}
function gccbutton_onkeydown()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").button_onkeydown(); 
}
function gcctextbox_onblur()
{
	var element = window.event.srcElement;
	element.getAttribute("calendar").textbox_onblur(); 
}
function gcctextbox_onkeydown()
{
	var element = window.event.srcElement;
	element.getAttribute("calendar").textbox_onkeydown(); 
}
function gcctextbox_onkeyup()
{
	var element = window.event.srcElement;
	element.getAttribute("calendar").textbox_onkeyup(); 
}
function gcctextbox_onchange()
{
	var element = window.event.srcElement;
	element.getAttribute("calendar").textbox_onchange();
}
function gccpreviousmonth_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").previousmonth_onclick();
}
function gccnextmonth_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").nextmonth_onclick(); 
}
function gccmonthcaption_onmousedown()
{
	var element = window.event.srcElement;	
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").monthcaption_onmousedown(); 
}
function gccday_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").day_onclick(); 
}
function gcccalendar_onblur(id)
{
	var calendar = document.getElementById(id); 
	calendar.getAttribute("calendar").calendar_onblur();
}
function gcccalendar_onkeydown(id)
{
	var calendar = document.getElementById(id);
	calendar.getAttribute("calendar").calendar_onkeydown(); 
}
function gcccalendar_onselectstart()
{
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return true; 
}
function gccscrollablemonths_onselectstart()
{
	window.event.cancelBubble = true;
	window.event.returnValue = false; 
	return true;
}
function GridEXCalendarComboDropDown(calendarID, monthNames, firstDayWeek)
{	
	if(monthNames == null || firstDayWeek == null)
		throw Error("arguments of GridEXCalendarComboDropDown constructor are null or invalid");
		
	var id = calendarID; 	
	var button = document.getElementById(calendarID + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXCalendarDropDown '" + calendarID + "'"); 
		
	if(button.parentElement != null && button.parentElement.tagName != "BODY")
		document.body.appendChild(button); 
		
	var textbox = document.getElementById(calendarID + "_textbox"); 
	if(textbox == null)
		throw Error("unable to find textbox for GridEXCalendarDropDown '" + calendarID + "'"); 
		
	if(textbox.parentElement != null && textbox.parentElement.tagName != "BODY")
		document.body.appendChild(textbox); 
		
	var calendar = document.getElementById(id); 
	if(calendar == null)
		throw Error("calendar is null or invalid");
		
	if(calendar.parentElement != null && calendar.parentElement.tagName != "BODY")
		document.body.appendChild(calendar);  
		
	var cellMonthCaption = null; 
	var daysTable = null;	
	var monthsScrollable = null; 
	cellMonthCaption = calendar.childNodes[0].childNodes[0].cells[1];
	daysTable = calendar.childNodes[1].childNodes[0]; 	
	var owner = null; 	
	var currMonth = -1;
	var currDay = -1; 
	var currYear = -1; 		
	var firstDayWeek = firstDayWeek;
	var monthNames = monthNames;	
	var left = -1; 
	var top = -1; 
	var width = -1;
	var height = -1;	
	var monthDays = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);	
	var selectedDate = null; 	
	this.getDatePattern = getDatePattern; 
	this.getID = getID;
	this.getInnerTextBox = getInnerTextBox; 
	this.getSelectedDateString = getSelectedDateString; 
	this.getSelectedDate = getSelectedDate;	
	this.setLeft = setLeft;	
	this.setTop = setTop; 
	this.setHeight = setHeight;
	this.setWidth = setWidth; 
	this.setOwner = setOwner; 			
	this.setSelectedDate = setSelectedDate;
	this.setStyle = setStyle; 	
	this.Hide = Hide; 	
	this.Show = Show;	
	this.Unload = Unload; 
	this.today_onclick = today_onclick; 
	this.none_onclick = none_onclick;
	this.button_onblur = button_onblur;
	this.button_onmousedown = button_onmousedown;
	this.button_onkeydown = button_onkeydown;
	this.textbox_onblur = textbox_onblur;
	this.textbox_onchange = textbox_onchange;
	this.textbox_onkeydown = textbox_onkeydown;
	this.textbox_onkeyup = textbox_onkeyup; 
	this.previousmonth_onclick = previousmonth_onclick;
	this.nextmonth_onclick = nextmonth_onclick; 
	this.monthcaption_onmousedown = monthcaption_onmousedown;
	this.day_onclick = day_onclick;
	this.calendar_onblur = calendar_onblur; 
	this.calendar_onkeydown = calendar_onkeydown; 
	function initializeMonthDays()
	{
		var _isLeapYear = false; 
		if((currYear % 4) == 0)
		{
			if(((currYear % 100) != 0) || ((currYear % 400) == 0))
				_isLeapYear = true;
		}		
		if(_isLeapYear)
			monthDays[1] = 29;
		else
			monthDays[1] = 28; 
	}	
	function getDaysInMonth(month)
	{	
		if(month < 0 || month >= 12)
			throw Error("invalid month number");
	
		return monthDays[month]; 
	}			
	function getMonth()
	{
		return currMonth; 
	}	
	function getMonthName(month)
	{
		if(month < 0 || month >= 12)
			throw Error("invalid month number"); 
			
		return monthNames[month]; 
	}	
	function getYear()
	{
		return currYear; 
	}	
	function isSelectedDate(day, month, year)
	{
		if(selectedDate == null)
			return false;
			
		return (selectedDate.getDate() == day && selectedDate.getMonth() == month && selectedDate.getFullYear() == year); 						
	}	
	function isTodayDate(date, day, month, year)
	{		
		return (date.getDate() == day && date.getMonth() == month && date.getFullYear() == year); 
	}	
	function getID()
	{
		return id; 
	}	
	function setOwner(value)
	{
		owner = value; 		
	}	
	function setSelectedDate(value)
	{
		if(value == null || value == "")
		{			
			selectedDate = null; 			
			var date = new Date(); 
			currDay = date.getDate(); 
			currMonth = date.getMonth(); 
			currYear = date.getFullYear(); 
		}
		else
		{		
			if(!isDate(value, getDatePattern()))
				throw Error("value argument is not a valid date");
				
			selectedDate = getDateFromFormat(value, getDatePattern()); 			
			updateTextBox(); 			
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear(); 						
		}
	}	
	function getInnerTextBox()
	{
		return textbox; 
	}	
	function getSelectedDate()
	{
		return selectedDate; 
	}	
	function getSelectedDateString()
	{
		if(selectedDate != null)
			return formatDate(getSelectedDate(), getDatePattern()); 
		else
			return ""; 
	}	
	function getDatePattern()
	{		
		var pattern = null; 
		if(owner != null)
			pattern = owner.getCell().getColumn().datePattern;
		if(pattern == null)
			return "dd/MM/yyyy"; 
		else
			return pattern;
	}	
	function setLeft(value)
	{
		left = value; 
	}	
	function setStyle(style)
	{
		if(style != null)
		{
			textbox.style.fontFamily = style.fontFamily;
			textbox.style.fontSize = style.fontSize; 
			textbox.style.textAlign = style.textAlign;
		}
	}	
	function setTop(value)
	{
		top = value; 
	}	
	function setWidth(value)
	{
		width = value; 
	}	
	function setHeight(value)
	{
		height = value;
	}		
	function differentDates(a, b)
	{
		if(a.getDay() == b.getDay() && a.getMonth() == b.getMonth() && a.getFullYear() == b.getFullYear())
			return false;
		else
			return true; 
	}
	function commitNewValue()	
	{
		if(isDate(textbox.value, getDatePattern()))
		{
			var date = getDateFromFormat(textbox.value, getDatePattern()); 
			if(differentDates(date, getSelectedDate()))
			{
				selectedDate = date; 				
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
					
				updateTextBox(); 
			}
		}
	}	
	function updateTextBox()
	{
		textbox.value = getSelectedDateString(); 		
	}		
	function updateDropDown()
	{
		if(textbox.value != getSelectedDateString())
		{	
			var date = null; 		
			if(!isDate(textbox.value, getDatePattern()))
			{
				date = getSelectedDate(); 
				if(date == null)
					date = new Date();				
			}
			else							
				date = getDateFromFormat(textbox.value, getDatePattern())			
			if(date != getSelectedDate())
			{
				selectedDate = date; 
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
			}			
			currDay = date.getDate(); 
			currMonth = date.getMonth();
			currYear = date.getFullYear(); 
		}	
		else if(selectedDate != null)
		{
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear(); 			
		}			
		initializeMonthDays(); 		
		UpdateCalendar(true);
		calendar.style.display = "block"; 
		calendar.style.visibility = "visible";
		var scrollheight = -1;
		var fixedtop = -1;
		scrollheight = document.body.clientHeight;
		if((top + height + 2) + calendar.offsetHeight > scrollheight)
			fixedtop = top - calendar.offsetHeight;
		if(fixedtop != -1)
			calendar.style.pixelTop = fixedtop;
		else
			calendar.style.pixelTop = (top + button.offsetHeight) + 1;
		var proposedLeft = (left + width) - calendar.offsetWidth; 
		if(proposedLeft < 0 && (owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") != "1"))
		{
			proposedLeft = 0; 
			proposedLeft += getPixelWidth(document.body.currentStyle.paddingLeft);
			proposedLeft += getPixelWidth(document.body.currentStyle.marginLeft); 
		}		
		calendar.style.pixelLeft = proposedLeft; 
	}
	function Unload()
	{	
		textbox.detachEvent("onblur", gcctextbox_onblur);
		textbox.detachEvent("onkeydown", gcctextbox_onkeydown);
		textbox.removeAttribute("calendar"); 			
		delete textbox;		
		textbox = null;
		calendar.childNodes[0].childNodes[0].cells[0].detachEvent("onclick", gccpreviousmonth_onclick);
		calendar.childNodes[0].childNodes[0].cells[0].removeAttribute("calendar"); 
		calendar.childNodes[0].childNodes[0].cells[1].detachEvent("onmousedown", gccmonthcaption_onmousedown);
		calendar.childNodes[0].childNodes[0].cells[1].removeAttribute("calendar"); 
		calendar.childNodes[0].childNodes[0].cells[2].detachEvent("onclick", gccnextmonth_onclick);
		calendar.childNodes[0].childNodes[0].cells[2].removeAttribute("calendar"); 
		calendar.detachEvent("onblur", gcccalendar_onblur);
		calendar.detachEvent("onkeydown", gcccalendar_onkeydown); 
		calendar.detachEvent("onselectstart", gcccalendar_onselectstart); 
		calendar.removeAttribute("calendar"); 
		if(calendar.childNodes.length == 3)
		{
			var cmdTable = calendar.childNodes[2].childNodes[0];
			for(var cmdIndex = 0; cmdIndex < cmdTable.cells.length; cmdIndex++)
			{
				cmd = cmdTable.cells[cmdIndex]; 
				if(cmd.type == "1")
				{
					cmd.detachEvent("onclick", gcctoday_onclick); 
					cmd.removeAttribute("calendar");
				}
				else if(cmd.type == "2")
				{
					cmd.detachEvent("onclick", gccnone_onclick); 
					cmd.removeAttribute("calendar"); 
				}
			}
		}
		delete calendar;
		calendar = null;
		button.detachEvent("button_onblur", gccbutton_onblur);
		button.detachEvent("button_onmousedown", gccbutton_onmousedown);
		button.detachEvent("button_onkeydown", gccbutton_onkeydown);
		button.removeAttribute("calendar"); 
		delete button;		
		button = null; 
		for(var dayRow = 1; dayRow < dayRows; dayRow++)
		{
			var row = daysTable.rows[dayRow];		
			var dayCells = row.cells.length - 1;
			for(var dayCell = 1; dayCell < dayCells; dayCell++)
			{
				row.cells[dayCell].detachEvent("onclick", gccday_onclick);
				row.cells[dayCell].removeAttribute("calendar"); 
			}
		}
		delete daysTable;
		daysTable = null; 
		delete gridEXCalendarDropDown;
		gridEXCalendarDropDown = null; 
	}		
	function UpdateCalendar(rebindYear)
	{	
		var date = new Date();		
		var cellFilled = 0; 		
		var firstDay = (new Date(currYear, currMonth, 1)).getDay();
		firstDay = ((firstDay - firstDayWeek) < 0) ? ((firstDay - firstDayWeek) + 7) :  firstDay - firstDayWeek;		
		var lastDay = getDaysInMonth(currMonth);
		var lastPreviousMonthDay = -1;
		var nextFirstMonthDay = 1; 		
		var previousMonth = -1;
		var previousYear = -1; 
		var nextMonth = -1;
		var nextYear = -1; 		
		if(firstDay != 0)
		{
			if(currMonth  == 0) // january - obtain december 
			{
				previousMonth = 11; 
				previousYear = getYear() - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth);
			}
			else
			{
				previousMonth = currMonth - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth); 
			}
		}		
		if(currMonth == 11) // december
		{
			nextYear = getYear() + 1; 
			nextMonth = 0; 
		}
		else
			nextMonth = currMonth + 1; 

		var dayCell = firstDay;		
		var day = 1;			
		var irow = 1; 
		var row = daysTable.rows[irow]; 		
		var cell = null; 		
		if(lastPreviousMonthDay != -1)
		{		
			var lastDayCell = dayCell-1;
			while(lastDayCell >= 0)
			{
				cell = row.cells[lastDayCell+1]; 
				cell.className = getID() + "_other_monthday";
				cell.innerHTML = lastPreviousMonthDay;
				cell.setAttribute("month", previousMonth); 				
				if(previousYear != -1)
					cell.setAttribute("year", previousYear);
				else
					cell.setAttribute("year", -1);

				lastDayCell--; 
				lastPreviousMonthDay--;
				cellFilled++; 
			}
		}		
		while(day <= lastDay)
		{
			if((dayCell != 0) && ((dayCell  % 7) == 0))
			{
				irow++;
				row = daysTable.rows[irow];
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 			
			if(isTodayDate(date, day, currMonth, currYear) && (selectedDate != null && !isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_today_notsel"; 				
			else if(isTodayDate(date, day, currMonth, currYear) && (selectedDate == null || isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_sel"; 
			else if(isSelectedDate(day, currMonth, currYear))
				cell.className = getID() + "_sel"; 
			else
				cell.className = getID() + "_monthday";

			cell.innerHTML = day;
			cell.setAttribute("month", getMonth()); 
			cell.setAttribute("year", -1); 
			day++; 
			dayCell++;
			cellFilled++; 
		}		
		while(cellFilled < 42)
		{			
			if((dayCell % 7) == 0)
			{
				irow++;
				row = daysTable.rows[irow]; 
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 
			cell.className = getID() + "_other_monthday"; 
			cell.innerHTML = nextFirstMonthDay; 
			cell.setAttribute("month", nextMonth); 
			if(nextYear != -1)
				cell.setAttribute("year", nextYear); 
			else
				cell.setAttribute("year", -1);

			dayCell++;
			cellFilled++;
			nextFirstMonthDay++; 
		}		
		cellMonthCaption.innerHTML = getMonthName(currMonth) + "&nbsp;" + currYear;
	}	
	function HideButton()
	{
		button.style.visibility = "hidden"; 
	}	
	function HideCalendar()
	{
		calendar.style.visibility = "hidden";
		hideCalendarBackFrame();
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.status = 0;
				img.src = owner.getCell().getGridEX().ddbimg;
			}
		}
	}
	function HideScrollableMonths()
	{
		monthsScrollable.style.visibility = "hidden"; 
		hideCalendarBackMonthsFrame();
		if(calendar.setActive != null)
			calendar.setActive(); 
	}
	function HideTextBox()
	{
		textbox.style.visibility = "hidden"; 
	}	
	function Hide()
	{
		HideCalendar(); 
		HideButton();
		HideTextBox(); 
		textbox.value  = ""; 
	}
	function Show()
	{
		button.style.visibility = "visible"; 
		button.style.pixelTop = top; 
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			button.style.pixelLeft = left;
		else
			button.style.pixelLeft = left + width - button.offsetWidth; 
		button.style.pixelHeight = height;		
		textbox.style.visibility = "visible"; 
		textbox.style.pixelTop = top; 
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			textbox.style.pixelLeft = left + button.offsetWidth;
		else
			textbox.style.pixelLeft = left;
		textbox.style.pixelHeight = height; 
		textbox.style.pixelWidth = (width - button.offsetWidth);
		textbox.focus(); 
	}	
	// private events 
	function button_onblur()
	{	
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 
	
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true; 
			
		Hide(); 
		return true; 
	}	
	function button_onmousedown()
	{
		if(calendar.style.visibility != "visible")
		{
			if(owner != null && owner.DropDown != null)
				owner.DropDown(); 
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.status = 1; 
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}				
			updateDropDown();
			showCalendarBackFrame(calendar.style.pixelLeft, calendar.style.pixelTop, calendar.offsetWidth, calendar.offsetHeight); 			
			calendar.focus(); 
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.status = 0;
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}
			HideCalendar(); 
		}
	}	
	function button_onkeydown()
	{
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown();
			
		if(window.event.keyCode == 33 || window.event.keyCode == 34)
			cancelEvent(); 
	}
	function textbox_onblur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true;
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
			
		Hide();
		return true; 
	}	
	function textbox_onchange()
	{		
		commitNewValue(); 
	}	
	function textbox_onkeydown()
	{
		if(window.event.keyCode == 13)
		{		
			commitNewValue();			
			window.event.returnValue = false; 
			window.event.cancelBubble = true;			
		}
		else if(window.event.keyCode == 9 || window.event.keyCode == 38 || window.event.keyCode == 40)
			commitNewValue(); 
			
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 
		
		return true; 
	}
	function textbox_onkeyup()
	{
		if(window.event.altKey && window.event.keyCode == 9)
			commitNewValue(); 
			
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp();
	}	
	function calendar_onblur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
		{
			HideCalendar(); 
			return true; 
		}			
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true; 
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving();
		else
			Hide(); 
			
		return true; 
	}	
	function calendar_onkeydown()
	{
		if(window.event.keyCode == 27 || window.event.keyCode == 13)
		{
			if(owner != null && owner.Leaving != null)
				owner.Leaving(); 
				
			HideCalendar(); 
			textbox.focus(); 			
			if(window.event.keyCode == 13)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
			}
		}
		else if(window.event.keyCode == 9)
		{
			Hide(); 
			if(owner != null && owner.KeyDown != null)
				owner.KeyDown(); 
		}
	}	
	function day_onclick()
	{
		var element = window.event.srcElement; 
		if(element == null || element.tagName != "TD" || element.innerHTML == "")
			return; 
			
		var dayCell = element; 		
		var _day = element.innerHTML;
		var _month = element.getAttribute("month"); 
		var _year = element.getAttribute("year");		
		var _selectedDate = new Date((_year == -1) ? getYear() : _year, (_month == -1) ? getMonth() : _month, _day);
		if(_selectedDate != getSelectedDate())
		{
			selectedDate = _selectedDate; 
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
				
			updateTextBox(); 
		}		
		HideCalendar(); 
		textbox.focus(); 
	}	
	var initializedMonthsScrollable = false;
	function RefreshMonthsScrollable()	
	{	
		var rowmonth = null; 			
		var scrollableTable = monthsScrollable.childNodes[0]; 
		rowmonth = scrollableTable.rows[3];
		rowmonth.setAttribute("imonth", getMonth()); 
		rowmonth.setAttribute("iyear", getYear()); 
		rowmonth.cells[0].innerHTML = getMonthName(getMonth()) + "&nbsp;" + getYear(); 		
		rowmonth.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = rowmonth; 
		var icount = 2; 
		var imonth = getMonth(); 
		var iyear = getYear(); 		
		while(icount >= 0)
		{
			if(imonth - 1 < 0)
			{
				imonth = 11;
				iyear = iyear -1; 
			}
			else
				imonth = imonth - 1; 
		
			rowmonth = scrollableTable.rows[icount];
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 
			icount = icount - 1; 
		}		
		icount = 4; 
		imonth = getMonth(); 
		iyear = getYear(); 
		while(icount <= 6)
		{		
			if(imonth + 1 > 11)
			{
				imonth = 0; 
				iyear = iyear + 1; 
			}	
			else
				imonth = imonth + 1; 
		
			rowmonth = scrollableTable.rows[icount]; 
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 			
			icount = icount + 1; 
		}		
	}
	function getMonthRowFromEvent()
	{
		var monthrow = null; 
		if(window.event.srcElement != null && window.event.srcElement.tagName == "TR")
			monthrow = window.event.srcElement; 
		else if(window.event.srcElement != null && window.event.srcElement.tagName == "TD")
			monthrow = window.event.srcElement.parentElement; 
		if(monthrow == null)
			throw Error("unable to get monthrow generator"); 
			
		return monthrow; 		
	}
	function month_onmouseover()
	{
		if(lastSelectedMonth != null && lastSelectedMonth.className != getID() + "_normalmonth")
			lastSelectedMonth.className = getID() + "_normalmonth"; 			

		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = monthrow; 		
	}
	function month_onmouseout()
	{
		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_normalmonth"; 
	}
	var lastSelectedMonth = null; 
	function RefreshSelectedMonth(monthrow)
	{
		var imonth = parseInt(monthrow.getAttribute("imonth"), 10); 
		var iyear = parseInt(monthrow.getAttribute("iyear"), 10); 
		if(imonth != getMonth() || iyear != getYear())
		{		
			currMonth = imonth; 
			currYear = iyear; 
			initializeMonthDays();
			UpdateCalendar(true); 
		}
		HideScrollableMonths(); 		
	}
	function month_onclick()
	{
		var monthrow = getMonthRowFromEvent(); 
		RefreshSelectedMonth(monthrow); 
		lastSelectedMonth = null; 
	}
	function scrollablemonths_onkeydown()
	{		
		if(window.event.keyCode == 27)
			HideScrollableMonths(); 
	}	
	var lastClientY = null; 
	function scrollablemonths_onmousemove()
	{		
		lastClientY = window.event.clientY; 
	}
	function scrollablemonths_onmouseup()
	{		
		if(intervalID != null)
		{
			window.clearInterval(intervalID); 
			if(window.event.srcElement != null && lastSelectedMonth != null)
			{
				if(window.event.srcElement == lastSelectedMonth || lastSelectedMonth.contains(window.event.srcElement))
				{
					RefreshSelectedMonth(lastSelectedMonth); 
					lastSelectedMonth = null; 
				}
				else
					HideScrollableMonths(); 
			}
			else
				HideScrollableMonths();

			intervalID = null; 
		}
	}
	var intervalID = null; 
	function monthcaption_onmousedown()
	{
		if(!initializedMonthsScrollable)
		{
			monthsScrollable = document.getElementById(getID() + "_scrollablemonths"); 
			if(monthsScrollable == null)
				throw Error("unable to find scrollable months list");
				
			monthsScrollable.attachEvent("onkeydown", scrollablemonths_onkeydown); 
			monthsScrollable.attachEvent("onselectstart", gccscrollablemonths_onselectstart); 			
			document.body.attachEvent("onmousemove", scrollablemonths_onmousemove);
			document.body.attachEvent("onmouseup", scrollablemonths_onmouseup);			
			for(var i = 0; i < monthsScrollable.childNodes[0].rows.length; i++)
			{				
				var _row = monthsScrollable.childNodes[0].rows[i];
				_row.attachEvent("onclick", month_onclick); 
				_row.attachEvent("onmouseover", month_onmouseover); 
				_row.attachEvent("onmouseout", month_onmouseout); 
			}			
			initializedMonthsScrollable = true; 
		}	
		if(intervalID == null)
			intervalID = window.setInterval(scrollMonths, 400);

		RefreshMonthsScrollable(); 
		monthsScrollable.style.pixelLeft = getPixelLeft(cellMonthCaption);
		monthsScrollable.style.pixelTop = getPixelTop(cellMonthCaption) - (monthsScrollable.offsetHeight / 2); 
		monthsScrollable.style.pixelWidth = cellMonthCaption.offsetWidth; 
		monthsScrollable.style.visibility = "visible"; 
	}
	function scrollMonths()
	{				
		if(lastClientY == null)
			return; 
			
		var scrollDirection = 0; 
		if(lastClientY < monthsScrollable.style.pixelTop)
			scrollDirection = -1; 
		else if(lastClientY > (monthsScrollable.style.pixelTop + monthsScrollable.offsetHeight))
			scrollDirection = 1; 			
		if(scrollDirection != 0)
		{			
			var scrollableTable = monthsScrollable.childNodes[0]; 
			var _row = null; 
			var imonth = -1;
			var iyear = -1; 
			for(var irow = 0; irow < scrollableTable.rows.length; irow++)
			{				
				_row = scrollableTable.rows[irow]; 
				imonth = parseInt(_row.getAttribute("imonth"), 10); 
				iyear = parseInt(_row.getAttribute("iyear"), 10);
				if(scrollDirection == -1)
				{
					if(imonth - 1 < 0)
					{
						imonth = 11; 
						iyear = iyear - 1; 
					}
					else
						imonth = imonth - 1; 
				}
				else
				{
					if(imonth + 1 > 11)
					{
						imonth = 0; 
						iyear = iyear + 1; 
					}
					else
						imonth = imonth + 1; 
				}
				_row.setAttribute("imonth", imonth); 
				_row.setAttribute("iyear", iyear); 
				_row.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 	
			}
		}
	}
	function nextmonth_onclick()
	{
		if(currMonth == 11)	
		{
			currYear++;
			currMonth = 0;
		}
		else
			currMonth++;

		initializeMonthDays();
		UpdateCalendar(false); 
		if(calendar.setActive != null)
			calendar.setActive(); 
	}	
	function previousmonth_onclick()
	{
		if(currMonth == 0)		
		{
			currYear = currYear - 1; 
			currMonth = 11; 
		}
		else
			currMonth--; 

		initializeMonthDays();
		UpdateCalendar(false);
		if(calendar.setActive != null)
			calendar.setActive();
	}	
	function today_onclick()
	{
		var _selectedDate = new Date();
		if(_selectedDate != getSelectedDate())
		{
			selectedDate = _selectedDate; 			
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
				
			updateTextBox(); 
		}		
		HideCalendar(); 
		textbox.focus(); 
	}	
	function none_onclick()
	{
		selectedDate = null; 		
		if(owner != null && owner.ValueChanged != null)
			owner.ValueChanged(); 
			
		updateTextBox(); 			
		HideCalendar();
		textbox.focus(); 
	}		
	calendar.attachEvent("onblur", function() { gcccalendar_onblur(id); }); 
	calendar.attachEvent("onkeydown", function() { gcccalendar_onkeydown(id); });	
	calendar.attachEvent("onselectstart", gcccalendar_onselectstart);
	calendar.setAttribute("calendar", this);
	button.attachEvent("onblur", gccbutton_onblur);
	button.attachEvent("onmousedown", gccbutton_onmousedown);
	button.attachEvent("onkeydown", gccbutton_onkeydown);
	button.setAttribute("calendar", this); 
	textbox.attachEvent("onblur", gcctextbox_onblur); 
	textbox.attachEvent("onkeydown", gcctextbox_onkeydown); 
	textbox.attachEvent("onkeyup", gcctextbox_onkeyup); 	
	textbox.attachEvent("onchange", gcctextbox_onchange);
	textbox.setAttribute("calendar", this); 
	var cmd = null;
	cmd = calendar.childNodes[0].childNodes[0].cells[0];
	cmd.attachEvent("onclick", gccpreviousmonth_onclick); 
	cmd.setAttribute("calendar", this);
	cmd = calendar.childNodes[0].childNodes[0].cells[2]; 
	cmd.attachEvent("onclick", gccnextmonth_onclick);
	cmd.setAttribute("calendar", this); 
	cmd = calendar.childNodes[0].childNodes[0].cells[1]; 
	cmd.attachEvent("onmousedown", gccmonthcaption_onmousedown);
	cmd.setAttribute("calendar", this); 
	var row = null; 	
	var dayCells = -1; 	
	var dayRows = daysTable.rows.length; 
	for(var dayRow = 1; dayRow < dayRows; dayRow++)
	{
		row = daysTable.rows[dayRow];		
		dayCells = row.cells.length - 1;
		for(var dayCell = 1; dayCell < dayCells; dayCell++)
		{
			row.cells[dayCell].attachEvent("onclick", gccday_onclick);
			row.cells[dayCell].setAttribute("calendar", this); 
		}
	}	
	if(calendar.childNodes.length == 3)
	{
		var cmdTable = calendar.childNodes[2].childNodes[0];
		for(var cmdIndex = 0; cmdIndex < cmdTable.cells.length; cmdIndex++)
		{
			cmd = cmdTable.cells[cmdIndex]; 
			if(cmd.type == "1")
			{
				cmd.attachEvent("onclick", gcctoday_onclick); 
				cmd.setAttribute("calendar", this);
			}
			else if(cmd.type == "2")
			{
				cmd.attachEvent("onclick", gccnone_onclick); 
				cmd.setAttribute("calendar", this); 
			}
		}
	}		
	var gridEXCalendarDropDown = this;	
	return this;
}
function gccalendar_onblur(id)
{
	var calendar = document.getElementById(id);
	calendar.getAttribute("calendar").calendar_onblur(); 
}
function gccalendar_onkeydown(id)
{
	var calendar = document.getElementById(id);
	calendar.getAttribute("calendar").calendar_onkeydown();
}
function gcpreviousmonth_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").previousmonth_onclick(); 
}
function gcnextmonth_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").nextmonth_onclick(); 
}
function gcmonthcaption_onmousedown()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").monthcaption_onmousedown();
}
function gcday_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").day_onclick(); 		
}
function gctoday_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").today_onclick();
}
function gcnone_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TD")
		element = element.parentElement;
	if(element != null)
		element.getAttribute("calendar").none_onclick(); 
}
function GridEXCalendarDropDown(calendarID, monthNames, firstDayWeek)
{	
	if(monthNames == null || firstDayWeek == null)
		throw Error("arguments of GridEXCalendarDropDown constructor are null or invalid");
		
	var id = calendarID;	
	var button = document.getElementById(calendarID + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXCalendarDropDown '" + calendarID +"'"); 
		
	if(button.parentElement != null && button.parentElement.tagName != "BODY")
		document.body.appendChild(button); 
		
	var calendar = document.getElementById(id); 
	if(calendar == null)
		throw Error("calendar is null or invalid"); 
		
	if(calendar.parentElement != null && calendar.parentElement.tagName != "BODY")
		document.body.appendChild(calendar); 
		
	var cellMonthCaption = null; 
	var daysTable = null;	
	cellMonthCaption = calendar.childNodes[0].childNodes[0].cells[1];
	daysTable = calendar.childNodes[1].childNodes[0]; 	
	var monthsScrollable = null; 
	var owner = null; 	
	var currMonth = -1;
	var currDay = -1; 
	var currYear = -1; 	
	var firstDayWeek = firstDayWeek;
	var monthNames = monthNames;	
	var left = -1; 
	var top = -1; 
	var width = -1;
	var height = -1;	
	var monthDays = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);	
	var selectedDate = null; 	
	// public properties	
	this.getDatePattern = getDatePattern; 
	this.getID = getID;
	this.getInnerHtml = getInnerHtml; 
	this.getSelectedDate = getSelectedDate;
	this.getSelectedDateString = getSelectedDateString; 	
	this.setLeft = setLeft;	
	this.setTop = setTop; 
	this.setHeight = setHeight;
	this.setWidth = setWidth; 
	this.setOwner = setOwner; 			
	this.setSelectedDate = setSelectedDate;	
	this.Hide = Hide; 	
	this.Show = Show;	
	this.calendar_onblur = calendar_onblur; 
	this.calendar_onkeydown = calendar_onkeydown; 
	this.button_onblur = button_onblur; 
	this.button_onkeydown = button_onkeydown;
	this.button_onmousedown = button_onmousedown; 
	this.previousmonth_onclick = previousmonth_onclick; 
	this.nextmonth_onclick = nextmonth_onclick; 
	this.monthcaption_onmousedown = monthcaption_onmousedown;
	this.day_onclick = day_onclick;
	this.today_onclick = today_onclick;
	this.none_onclick = none_onclick;
	function initializeMonthDays()
	{
		var _isLeapYear = false; 
		if((currYear % 4) == 0)
		{
			if(((currYear % 100) != 0) || ((currYear % 400) == 0))
				_isLeapYear = true;
		}		
		if(_isLeapYear)
			monthDays[1] = 29;
		else
			monthDays[1] = 28; 
	}		
	function getDaysInMonth(month)
	{	
		if(month < 0 || month >= 12)
			throw Error("invalid month number");
	
		return monthDays[month]; 
	}			
	function getMonth()
	{
		return currMonth; 
	}	
	function getMonthName(month)
	{
		if(month < 0 || month >= 12)
			throw Error("invalid month number"); 
			
		return monthNames[month]; 
	}	
	function getYear()
	{
		return currYear; 
	}	
	function isSelectedDate(day, month, year)
	{
		if(selectedDate == null)
			return false;
			
		return (selectedDate.getDate() == day && selectedDate.getMonth() == month && selectedDate.getFullYear() == year); 						
	}	
	function isTodayDate(date, day, month, year)
	{		
		return (date.getDate() == day && date.getMonth() == month && date.getFullYear() == year); 
	}		
	function getID()
	{
		return id; 
	}	
	function getInnerHtml()
	{
		return calendar; 
	}
	function setOwner(value)
	{
		owner = value; 		
	}	
	function setSelectedDate(value)
	{
		if(value == null || value == "")
		{			
			selectedDate = null; 			
			var date = new Date(); 
			currDay = date.getDate(); 
			currMonth = date.getMonth(); 
			currYear = date.getFullYear(); 
		}
		else
		{		
			if(!isDate(value, getDatePattern()))
				throw Error("value argument is not a valid date");
				
			selectedDate = getDateFromFormat(value, getDatePattern()); 
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear(); 
		}
	}	
	function getSelectedDate()
	{
		return selectedDate; 
	}	
	function getSelectedDateString()
	{
		if(selectedDate != null)
			return formatDate(getSelectedDate(), getDatePattern()); 
		else
			return ""; 
	}	
	function getDatePattern()
	{		
		var pattern = null; 
		if(owner != null)
			pattern = owner.getCell().getColumn().datePattern;
		if(pattern == null)
			return "dd/MM/yyyy"; 
		else
			return pattern;	
	}	
	function setLeft(value)
	{
		left = value; 
	}	
	function setTop(value)
	{
		top = value; 
	}	
	function setWidth(value)
	{
		width = value; 
	}	
	function setHeight(value)
	{
		height = value;
	}		
	function updateDropDown()
	{		
		if(selectedDate != null)
		{
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear();
		}			
		initializeMonthDays(); 		
		UpdateCalendar(true); 		
		calendar.style.display = "block"; 
		calendar.style.visibility = "visible";
		var scrollheight = -1;
		var fixedtop = -1;
		scrollheight = document.body.clientHeight;
		if((top + height + 2) + calendar.offsetHeight > scrollheight)
			fixedtop = top - calendar.offsetHeight;
		if(fixedtop != -1)
			calendar.style.pixelTop = fixedtop; 
		else
			calendar.style.pixelTop = (top + button.offsetHeight) + 1;
		var proposedLeft = (left + width) - calendar.offsetWidth; 
		if(proposedLeft < 0 && (owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") != "1"))
		{
			proposedLeft = 0; 
			proposedLeft += getPixelWidth(document.body.currentStyle.paddingLeft); 
			proposedLeft += getPixelWidth(document.body.currentStyle.marginLeft); 
		}
		calendar.style.pixelLeft = proposedLeft; 		
		calendar.focus(); 								
	}			
	function UpdateCalendar(rebindYear)
	{	
		var date = new Date();		
		var cellFilled = 0; 		
		var firstDay = (new Date(currYear, currMonth, 1)).getDay();
		firstDay = ((firstDay - firstDayWeek) < 0) ? ((firstDay - firstDayWeek) + 7) :  firstDay - firstDayWeek;		
		var lastDay = getDaysInMonth(currMonth);
		var lastPreviousMonthDay = -1;
		var nextFirstMonthDay = 1; 		
		var previousMonth = -1;
		var previousYear = -1; 
		var nextMonth = -1;
		var nextYear = -1; 		
		if(firstDay != 0)
		{
			if(currMonth  == 0) // january - obtain december 
			{
				previousMonth = 11; 
				previousYear = getYear() - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth);
			}
			else
			{
				previousMonth = currMonth - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth); 
			}
		}		
		if(currMonth == 11) // december
		{
			nextYear = getYear() + 1; 
			nextMonth = 0; 
		}
		else
			nextMonth = currMonth + 1; 
		
		var dayCell = firstDay;		
		var day = 1;			
		var irow = 1; 
		var row = daysTable.rows[irow]; 		
		var cell = null; 		
		if(lastPreviousMonthDay != -1)
		{		
			var lastDayCell = dayCell-1;
			while(lastDayCell >= 0)
			{
				cell = row.cells[lastDayCell+1]; 
				cell.className = getID() + "_other_monthday";
				cell.innerHTML = lastPreviousMonthDay;
				cell.setAttribute("month", previousMonth); 				
				if(previousYear != -1)
					cell.setAttribute("year", previousYear);
				else
					cell.setAttribute("year", -1);
				lastDayCell--; 
				lastPreviousMonthDay--;
				cellFilled++; 
			}
		}		
		while(day <= lastDay)
		{
			if((dayCell != 0) && ((dayCell  % 7) == 0))
			{
				irow++;
				row = daysTable.rows[irow];
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 			
			if(isTodayDate(date, day, currMonth, currYear) && (selectedDate != null && !isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_today_notsel"; 				
			else if(isTodayDate(date, day, currMonth, currYear) && (selectedDate == null || isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_sel"; 
			else if(isSelectedDate(day, currMonth, currYear))
				cell.className = getID() + "_sel"; 
			else
				cell.className = getID() + "_monthday";
				
			cell.innerHTML = day;
			cell.setAttribute("month", getMonth()); 
			cell.setAttribute("year", -1); 
			day++; 
			dayCell++;
			cellFilled++; 
		}		
		while(cellFilled < 42)
		{			
			if((dayCell % 7) == 0)
			{
				irow++;
				row = daysTable.rows[irow]; 
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 
			cell.className = getID() + "_other_monthday"; 
			cell.innerHTML = nextFirstMonthDay; 
			cell.setAttribute("month", nextMonth); 
			if(nextYear != -1)
				cell.setAttribute("year", nextYear); 
			else
				cell.setAttribute("year", -1);
			dayCell++;
			cellFilled++;
			nextFirstMonthDay++; 
		}
		cellMonthCaption.innerHTML = getMonthName(currMonth) + "&nbsp;" + currYear;
	}	
	function HideButton()
	{
		button.style.visibility = "hidden"; 
	}	
	function HideCalendar()
	{
		calendar.style.visibility = "hidden"; 
		hideCalendarBackFrame();
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.status = 0;
				img.src = owner.getCell().getGridEX().ddbimg;
			}
		}
	}	
	function HideScrollableMonths()
	{
		monthsScrollable.style.visibility = "hidden"; 
		hideCalendarBackMonthsFrame();
		if(calendar.setActive != null)
			calendar.setActive();
	}	
	function Hide()
	{
		HideCalendar(); 
		HideButton() ;
	}					
	function Show()
	{
		button.style.visibility = "visible"; 
		button.style.pixelTop = top; 
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			button.style.pixelLeft = left; 
		else
			button.style.pixelLeft = left + width - button.offsetWidth; 
		button.style.pixelHeight = height; 
		button.focus(); 
	}	
	var initializedMonthsScrollable = false;
	function RefreshMonthsScrollable()	
	{	
		var rowmonth = null; 			
		var scrollableTable = monthsScrollable.childNodes[0]; 
		rowmonth = scrollableTable.rows[3];
		rowmonth.setAttribute("imonth", getMonth()); 
		rowmonth.setAttribute("iyear", getYear()); 
		rowmonth.cells[0].innerHTML = getMonthName(getMonth()) + "&nbsp;" + getYear(); 		
		rowmonth.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = rowmonth; 
		var icount = 2; 
		var imonth = getMonth(); 
		var iyear = getYear(); 		
		while(icount >= 0)
		{
			if(imonth - 1 < 0)
			{
				imonth = 11;
				iyear = iyear -1; 
			}
			else
				imonth = imonth - 1; 
			rowmonth = scrollableTable.rows[icount];
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 
			icount = icount - 1; 
		}		
		icount = 4; 
		imonth = getMonth(); 
		iyear = getYear(); 
		while(icount <= 6)
		{		
			if(imonth + 1 > 11)
			{
				imonth = 0; 
				iyear = iyear + 1; 
			}	
			else
				imonth = imonth + 1; 
			rowmonth = scrollableTable.rows[icount]; 
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 			
			icount = icount + 1; 
		}		
	}
	function getMonthRowFromEvent()
	{
		var monthrow = null; 
		if(window.event.srcElement != null && window.event.srcElement.tagName == "TR")
			monthrow = window.event.srcElement; 
		else if(window.event.srcElement != null && window.event.srcElement.tagName == "TD")
			monthrow = window.event.srcElement.parentElement; 
		if(monthrow == null)
			throw Error("unable to get monthrow generator"); 
			
		return monthrow; 		
	}
	function month_onmouseover()
	{
		if(lastSelectedMonth != null && lastSelectedMonth.className != getID() + "_normalmonth")
			lastSelectedMonth.className = getID() + "_normalmonth"; 			
		
		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = monthrow; 		
	}
	function month_onmouseout()
	{
		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_normalmonth"; 
	}
	var lastSelectedMonth = null; 
	function RefreshSelectedMonth(monthrow)
	{
		var imonth = parseInt(monthrow.getAttribute("imonth"), 10); 
		var iyear = parseInt(monthrow.getAttribute("iyear"), 10); 
		if(imonth != getMonth() || iyear != getYear())
		{		
			currMonth = imonth; 
			currYear = iyear; 
			initializeMonthDays();
			UpdateCalendar(true); 
		}
		HideScrollableMonths(); 		
	}
	function month_onclick()
	{
		var monthrow = getMonthRowFromEvent(); 
		RefreshSelectedMonth(monthrow); 
		lastSelectedMonth = null; 
	}
	function scrollablemonths_onkeydown()
	{		
		if(window.event.keyCode == 27)
			HideScrollableMonths(); 
	}	
	var lastClientY = null; 
	function scrollablemonths_onmousemove()
	{		
		lastClientY = window.event.clientY; 
	}
	function scrollablemonths_onmouseup()
	{		
		if(intervalID != null)
		{
			window.clearInterval(intervalID); 
			if(window.event.srcElement != null && lastSelectedMonth != null)
			{
				if(window.event.srcElement == lastSelectedMonth || lastSelectedMonth.contains(window.event.srcElement))
				{
					RefreshSelectedMonth(lastSelectedMonth); 
					lastSelectedMonth = null; 
				}
				else
					HideScrollableMonths(); 
			}
			else
				HideScrollableMonths();
			intervalID = null; 
		}
	}
	function scrollablemonths_onselectstart()
	{
		window.event.cancelBubble = true;
		window.event.returnValue = false; 
		return true;
	}
	var intervalID = null; 
	function monthcaption_onmousedown()
	{
		if(!initializedMonthsScrollable)
		{
			monthsScrollable = document.getElementById(getID() + "_scrollablemonths"); 
			if(monthsScrollable == null)
				throw Error("unable to find scrollable months list");
				
			monthsScrollable.attachEvent("onkeydown", scrollablemonths_onkeydown); 
			monthsScrollable.attachEvent("onselectstart", scrollablemonths_onselectstart); 			
			document.body.attachEvent("onmousemove", scrollablemonths_onmousemove);
			document.body.attachEvent("onmouseup", scrollablemonths_onmouseup);			
			for(var irow = 0; irow < monthsScrollable.childNodes[0].rows.length; irow++)
			{				
				var _row = monthsScrollable.childNodes[0].rows[irow];
				_row.attachEvent("onclick", month_onclick); 
				_row.attachEvent("onmouseover", month_onmouseover); 
				_row.attachEvent("onmouseout", month_onmouseout); 
			}			
			initializedMonthsScrollable = true; 
		}	
		if(intervalID == null)
			intervalID = window.setInterval(scrollMonths, 400);
		
		RefreshMonthsScrollable(); 
		monthsScrollable.style.pixelLeft = getPixelLeft(cellMonthCaption);
		monthsScrollable.style.pixelTop = getPixelTop(cellMonthCaption) - (monthsScrollable.offsetHeight / 2); 
		monthsScrollable.style.pixelWidth = cellMonthCaption.offsetWidth; 
		monthsScrollable.style.visibility = "visible"; 
		showCalendarBackMonthsFrame(monthsScrollable.style.pixelLeft, monthsScrollable.style.pixelTop, monthsScrollable.style.pixelWidth, monthsScrollable.offsetHeight); 
	}
	function scrollMonths()
	{				
		if(lastClientY == null)
			return; 
			
		var scrollDirection = 0; 
		if(lastClientY < monthsScrollable.style.pixelTop)
			scrollDirection = -1; 
		else if(lastClientY > (monthsScrollable.style.pixelTop + monthsScrollable.offsetHeight))
			scrollDirection = 1; 			
		if(scrollDirection != 0)
		{			
			var scrollableTable = monthsScrollable.childNodes[0]; 
			var _row = null; 
			var imonth = -1;
			var iyear = -1; 
			for(var irow = 0; irow < scrollableTable.rows.length; irow++)
			{				
				_row = scrollableTable.rows[irow]; 
				imonth = parseInt(_row.getAttribute("imonth"), 10); 
				iyear = parseInt(_row.getAttribute("iyear"), 10);
				if(scrollDirection == -1)
				{
					if(imonth - 1 < 0)
					{
						imonth = 11; 
						iyear = iyear - 1; 
					}
					else
						imonth = imonth - 1; 
				}
				else
				{
					if(imonth + 1 > 11)
					{
						imonth = 0; 
						iyear = iyear + 1; 
					}
					else
						imonth = imonth + 1; 
				}
				_row.setAttribute("imonth", imonth); 
				_row.setAttribute("iyear", iyear); 
				_row.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 	
			}
		}
	}
	function button_onblur()
	{
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true; 
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
		else					
			Hide(); 
			
		return true; 
	}	
	function button_onkeydown()
	{	
		if(window.event.keyCode == 13 || window.event.keyCode == 33 || window.event.keyCode == 34)
		{	
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
		}		
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 
	}	
	function button_onmousedown()
	{		
		if(calendar.style.visibility != "visible")
		{
			if(owner != null && owner.DropDown != null)
				owner.DropDown(); 
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.status = 1; 
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}				
			updateDropDown(); 					
			showCalendarBackFrame(calendar.style.pixelLeft, calendar.style.pixelTop, calendar.offsetWidth, calendar.offsetHeight); 			
			calendar.focus(); 
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.status = 0;
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}
			HideCalendar(); 
		}
	}	
	function calendar_onblur()
	{	
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true; 
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 						
		else
			Hide();
			
		return true; 
	}	
	function calendar_onkeydown()
	{
		if(window.event.keyCode == 13)
		{
			HideCalendar();
			button.focus();			
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
		}
		else if(window.event.keyCode == 27)
		{
			HideCalendar(); 
			button.focus(); 			
		}
		else if(window.event.keyCode == 9)
		{
			Hide(); 
			if(owner != null && owner.KeyDown != null)
				owner.KeyDown(); 
		}
	}	
	function day_onclick()
	{
		var element = window.event.srcElement; 
		if(element == null || element.tagName != "TD" || element.innerHTML == "")
			return; 
			
		var dayCell = element; 		
		var _day = element.innerHTML;
		var _month = element.getAttribute("month"); 
		var _year = element.getAttribute("year");		
		var _selectedDate = new Date((_year == -1) ? getYear() : _year, (_month == -1) ? getMonth() : _month, _day);
		var previousDate = getSelectedDate(); 
		if(_selectedDate != previousDate)
		{
			selectedDate = _selectedDate; 
			if(owner != null && owner.ValueChanged != null)
			{
				var x = owner.ValueChanged(); 
				if(x != null && x == true)
					selectedDate = previousDate; 
			}
		}		
		HideCalendar(); 
		button.focus(); 
	}	
	function nextmonth_onclick()
	{
		if(currMonth == 11)	
		{
			currYear++;
			currMonth = 0;
		}
		else 
			currMonth++; 
		
		initializeMonthDays();
		UpdateCalendar(false); 
		if(calendar.setActive != null)
			calendar.setActive(); 
	}	
	function previousmonth_onclick()
	{
		if(currMonth == 0)		
		{
			currYear = currYear - 1; 
			currMonth = 11; 
		}
		else
			currMonth--; 
		
		initializeMonthDays();
		UpdateCalendar(false);
		if(calendar.setActive != null)
			calendar.setActive(); 
	}	
	function today_onclick()
	{
		var _selectedDate = new Date();
		var previousDate = getSelectedDate(); 
		if(_selectedDate != previousDate)
		{
			selectedDate = _selectedDate;
			if(owner != null && owner.ValueChanged != null)
			{
				var x = owner.ValueChanged();
				if(x != null && x == true)
					selectedDate = previousDate;
			}
		}		
		HideCalendar(); 
		button.focus(); 
	}	
	function none_onclick()
	{
		selectedDate = null; 		
		var previousDate = getSelectedDate(); 
		if(owner != null && owner.ValueChanged != null)
		{
			var x = owner.ValueChanged(); 
			if(x != null && x == true)
				selectedDate = previousDate; 
		}			
		HideCalendar();
		button.focus(); 
	}	
	calendar.attachEvent("onblur", function() { gccalendar_onblur(id); } ); 
	calendar.attachEvent("onkeydown", function() { gccalendar_onkeydown(id); } );	
	calendar.setAttribute("calendar", this); 
	button.attachEvent("onblur", gccbutton_onblur); 
	button.attachEvent("onkeydown", gccbutton_onkeydown); 
	button.attachEvent("onmousedown", gccbutton_onmousedown); 	
	button.setAttribute("calendar", this); 
	var cmd = null; 	
	cmd = calendar.childNodes[0].childNodes[0].cells[0];
	cmd.attachEvent("onclick", gcpreviousmonth_onclick); 
	cmd.setAttribute("calendar", this); 
	cmd = calendar.childNodes[0].childNodes[0].cells[2]; 
	cmd.attachEvent("onclick", gcnextmonth_onclick);
	cmd.setAttribute("calendar", this); 
	cmd = calendar.childNodes[0].childNodes[0].cells[1]; 
	cmd.attachEvent("onmousedown", gcmonthcaption_onmousedown);	
	cmd.setAttribute("calendar", this); 
	var row = null; 	
	var dayCells = -1; 	
	var dayRows = daysTable.rows.length; 
	for(var dayRow = 1; dayRow < dayRows; dayRow++)
	{
		row = daysTable.rows[dayRow];		
		dayCells = row.cells.length - 1;
		for(var dayCell = 1; dayCell < dayCells; dayCell++)
		{
			row.cells[dayCell].attachEvent("onclick", gcday_onclick); 
			row.cells[dayCell].setAttribute("calendar", this); 
		}
	}	
	if(calendar.childNodes.length == 3)
	{
		var cmdTable = calendar.childNodes[2].childNodes[0];
		for(var cmdIndex = 0; cmdIndex < cmdTable.cells.length; cmdIndex++)
		{
			cmd = cmdTable.cells[cmdIndex]; 
			if(cmd.type == "1")
			{
				cmd.attachEvent("onclick", gctoday_onclick); 
				cmd.setAttribute("calendar", this);
			}
			else if(cmd.type == "2")
			{
				cmd.attachEvent("onclick", gcnone_onclick); 
				cmd.setAttribute("calendar", this); 
			}
		}
	}		
	var gridEXCalendarDropDown = this;	
	return this; 
}