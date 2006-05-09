///////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////
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
function _isAlphaCharacter(val)
{
	return ((val >= 65 && val <= 90) || (val >= 97 && val <= 122));
}
function ccFormatDate(date, format, names) 
{
	format=format+"";
	var result="";
	var i_format=0;
	var c="";
	var token="";
	var y=date.getFullYear()+"";
	var M=date.getMonth()+1;
	var d=date.getDate();
	var E=date.getDay();
	var H=date.getHours();
	var m=date.getMinutes();
	var s=date.getSeconds();
	var yyyy,yy,MMM,MM,dd,hh,h,mm,ss,ampm,HH,H,KK,K,kk,k;
	var MONTH_NAMES = names[0];
	var MONTH_SHORT_NAMES = names[1]; 
	var DAY_NAMES = names[2]; 
	var DAY_SHORT_NAMES = names[3]; 
	var value=new Object();
	if (y.length < 4) {y=""+(y-0+1900);}
	value["y"]=""+y;
	value["yyyy"]=y;
	value["yy"]=y.substring(2,4);
	value["M"]=M;
	value["MM"]=LZ(M);
	value["MMM"]=MONTH_SHORT_NAMES[M-1];
	value["MMMM"]=MONTH_NAMES[M-1];	
	value["d"]=d;
	value["dd"]=LZ(d);
	value["ddd"]=DAY_SHORT_NAMES[E];	
	value["dddd"]=DAY_NAMES[E];
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
	while (i_format < format.length) {
		c=format.charAt(i_format);
		token="";	
		while(_isAlphaCharacter(format.charCodeAt(i_format)) && (i_format < format.length)) {
			token += format.charAt(i_format++);		
		}
		if(token != "")
		{				
			if (value[token] != null) { result=result + value[token]; }
			else { result=result + token; }
		}
		else if(i_format < format.length)
			result = result + format.charAt(i_format++); 		
		}
	return result;
}
function ccGetDateFromFormat(val, format, names) 
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
	
	while (i_format < format.length) 
	{
		c=format.charAt(i_format);
		token="";
		while (_isAlphaCharacter(format.charCodeAt(i_format)) && (i_format < format.length)) 
		{ 
			token += format.charAt(i_format++);
		}
		if (token=="yyyy" || token=="yy" || token=="y") 
		{
			if (token=="yyyy") { x=4;y=4; }
			if (token=="yy")   { x=2;y=2; }
			if (token=="y")    { x=2;y=4; }
			year=_getInt(val,i_val,x,y);
			if (year==null) { return 0; }
			i_val += year.length;
			if (year.length==2) {
				if (year > 70) { year=1900+(year-0); }
				else { year=2000+(year-0); }
				}
		}
		else if (token=="MMM" || token == "MMMM")
		{
			month=0;
			var array = null;
			if(token == "MMM")
				array = names[1];
			else
				array = names[0];
			for (var i=0; i<array.length; i++) 
			{
				var month_name=array[i];
				if (val.substring(i_val,i_val+month_name.length).toLowerCase()==month_name.toLowerCase()) 
				{					
					month=i+1;						
					if(month > 12)
						month -= 12;
					i_val += month_name.length;
					break;
				}
			}
			if ((month < 1)||(month>12))
				return 0;
		}		
		else if (token=="ddd" || token=="dddd")
		{
			var array = null;
			if(token == "ddd")
				array = names[3];
			else
				array = names[2];
			for (var i=0; i<array.length; i++) 
			{
				var day_name=array[i];
				if (val.substring(i_val,i_val+day_name.length).toLowerCase()==day_name.toLowerCase()) 
				{
					i_val += day_name.length;
					break;
				}
			}
		}
		else if (token=="MM"||token=="M") 
		{
			month=_getInt(val,i_val,token.length,2);
			if(month==null||(month<1)||(month>12)){return 0;}
			i_val+=month.length;
		}
		else if (token=="dd"||token=="d") 
		{
			date=_getInt(val,i_val,token.length,2);
			if(date==null||(date<1)||(date>31)){return 0;}
			i_val+=date.length;
		}
		else if (token=="hh"||token=="h") 
		{
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<1)||(hh>12)){return 0;}
			i_val+=hh.length;
		}
		else if (token=="HH"||token=="H") 
		{
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<0)||(hh>23)){return 0;}
			i_val+=hh.length;
		}
		else if (token=="KK"||token=="K") 
		{
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<0)||(hh>11)){return 0;}
			i_val+=hh.length;
		}
		else if (token=="kk"||token=="k") 
		{
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<1)||(hh>24)){return 0;}
			i_val+=hh.length;hh--;
		}
		else if (token=="mm"||token=="m") 
		{
			mm=_getInt(val,i_val,token.length,2);
			if(mm==null||(mm<0)||(mm>59)){return 0;}
			i_val+=mm.length;
		}
		else if (token=="ss"||token=="s") 
		{
			ss=_getInt(val,i_val,token.length,2);
			if(ss==null||(ss<0)||(ss>59)){return 0;}
			i_val+=ss.length;
		}
		else if (token=="a") 
		{
			if (val.substring(i_val,i_val+2).toLowerCase()=="am") {ampm="AM";}
			else if (val.substring(i_val,i_val+2).toLowerCase()=="pm") {ampm="PM";}
			else {return 0;}
			i_val+=2;
		}
		else 
		{
			if (val.substring(i_val,i_val+token.length)!=token)
				return 0;
			else
			{
				if(token.length > 0)
					i_val+=token.length;
				else
				{
					i_val++;
					i_format++;
				}
			}
		}
	}
	if (i_val != val.length) { return 0; }
	if (month==2) {
		if ( ( (year%4==0)&&(year%100 != 0) ) || (year%400==0) ) { 
			if (date > 29){ return 0; }
			}
		else { if (date > 28) { return 0; } }
		}
	if ((month==4)||(month==6)||(month==9)||(month==11)) {
		if (date > 30) { return 0; }
		}
	if (hh<12 && ampm=="PM") { hh=hh-0+12; }
	else if (hh>11 && ampm=="AM") { hh-=12; }
	var newdate=new Date(year,month-1,date,hh,mm,ss);
	return newdate; 
}
function calendarComboOnLoad(id)
{
	var calendarCombo = getCalendarCombo(id); 
	calendarCombo.onLoad(); 
}
function calendarComboOnBlur(id)
{
	var calendarCombo = getCalendarCombo(id); 	
	calendarCombo.onBlur(); 
}
function mouseMoveScrollableMonths(id)
{
	var calendarCombo = getCalendarCombo(id);
	calendarCombo.onMouseMoveScrollableMonths(); 
}
function mouseUpScrollableMonths(id)
{
	var calendarCombo = getCalendarCombo(id);
	calendarCombo.onMouseUpScrollableMonths(); 
}
function Pair()
{
	var first = null;
	var second = null; 
	this.First = first;
	this.Second = second;
	return this; 
}
function replaceInstances(value, find, replace)
{
	while(value.indexOf(find) >= 0)
	{
		value = value.replace(find, replace);
	}
	return value; 
}
function normalizeFormat(format)
{
	format = replaceInstances(format, "&apos;", "'"); 
	return format; 
}
function CalendarCombo(id, css, columns, rows, weeknumber, weekrule, clientvalue, arrbd, arrabd, arrmd, events, names, firstdayweek, dateformat, dateincrements, isnull, sadd, apb, readonly)
{
	var autoPB = (apb == 1); 
	var id = id; 
	var readonly = (readonly == 1); 
	var dateincrements = dateincrements;
	var events = events; 	
	var namesArray = names; 
	var monthnames = namesArray[0];
	var firstdayweek = firstdayweek; 
	var weeknumber = (weeknumber == 1) ? true : false; 
	var weekrule = weekrule; 
	var dateformat  = normalizeFormat(dateformat); 
	var calendarCSS = css; 		
	var selectedDate = new Date(clientvalue[0], clientvalue[1], clientvalue[2]); 
	var bindableDate = selectedDate; 
	if(isnull == 1)		
		selectedDate = null; 
	var showAsDropDown = sadd;
	var minDate = new Date(clientvalue[3], clientvalue[4], clientvalue[5]); 
	var maxDate = new Date(clientvalue[6], clientvalue[7], clientvalue[8]); 
	this.Columns = columns;
	this.Rows = rows;
	this.ShowNextMonth = ShowNextMonth; 
	this.ShowPreviousMonth = ShowPreviousMonth; 	
	this.ShowScrollableMonths = ShowScrollableMonths;
	this.ShowDropDown = ShowDropDown; 
	this.MinDate = minDate;
	this.MaxDate = maxDate; 
	this.Value = selectedDate; 
	this.onMouseMoveScrollableMonths = onMouseMoveScrollableMonths;
	this.onMouseUpScrollableMonths = onMouseUpScrollableMonths; 
	this.onClickDay = onClickDay; 
	this.onClickMonth = onClickMonth;
	this.onClickNone = onClickNone; 
	this.onClickToday = onClickToday;
	this.onBlurDateInput = function() { unselectCurrentSpan(); } 
	this.onClickDateInput = onClickDateInput; 
	this.onChangeDateInput = onChangeDateInput; 
	this.onKeyDownDateInput = onKeyDownDateInput;
	this.onBlur = onBlur; 
	this.onLoad = onLoad;
	this.onMouseOverToday = onMouseOverToday;
	this.onMouseOutToday = onMouseOutToday; 
	this.onMouseOverNone = onMouseOverNone;
	this.onMouseOutNone = onMouseOutNone; 
	this.onMouseOverMonth =onMouseOverMonth;
	this.onMouseOutMonth = onMouseOutMonth;
	this.onMouseOutDropDown = onMouseOutDropDown; 
	this.onMouseOverDropDown = onMouseOverDropDown;	
	var boldedDates = getDatesArray(arrbd);
	var anuallyBoldedDates = getDatesArray(arrabd); 
	var monthDates = getDatesArray(arrmd, columns, rows);			
	function updateStatus()
	{
		var element = document.getElementsByName(id + "_status")[0]; 
		element.value = monthDates[0].getFullYear() + "," + (monthDates[0].getMonth()+1); 
		element.value += "|"; 
		if(selectedDate == null)
			element.value += "null"; 
		else
			element.value += selectedDate.getFullYear() + "," + (selectedDate.getMonth()+1) + "," + selectedDate.getDate(); 
	}
	var tdSelected = null; 
	function isMonthEqual(dateA, dateB)
	{
		if(dateA.getFullYear() == dateB.getFullYear())
		{
			if(dateA.getMonth() == dateB.getMonth())
				return 0;
			else if(dateA.getMonth() < dateB.getMonth())
				return -1;
			else 
				return 1;			
		}
		else if(dateA.getFullYear() < dateB.getFullYear() && dateA.getMonth() == 11)
			return -1;
		else if(dateB.getFullYear() > dateA.getFullYear() && dateB.getMonth() == 0)
			return 1; 
		else if(dateA.getFullYear() > dateB.getFullYear())
			return 1; 
	}
	function onBlur()
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
	function doPostBack(eventArgument)
	{
		updateStatus(); 
		__doPostBack(document.getElementById(id).getAttribute("name"), eventArgument); 
	}
	var refreshCalendar = false; 
	function onClickToday(button, postback)
	{		
		if(readonly)
			return; 
		if(isDropDownVisible())
		{			
			var d = new Date();
			if(d >= minDate && d <= maxDate)
			{
				setSelectedDate(d); 
				FireEvent("TodayButtonClick", [calendarCombo]); 			
				updateDateIndicator();
				refreshCalendar = true;
				hideDropDown(); 			
				if(postback == 1 || autoPB)
					doPostBack("TodayButtonClick"); 
			}
		}
	}
	var dayToCommit = -1;
	var monthToCommit = -1;
	var yearToCommit = -1;
	var currentSpan = null; 
	var currentInput = -1; 
	var bufferLength = -1;
	var bufferString = "";
	var bufferMonthString = "";
	var numericValue = -1; 
	function unselectCurrentSpan()
	{
		if(currentSpan != null)
			currentSpan.style.backgroundColor = currentSpan.style.color = ""; 			
		currentSpan = null; 
	}
	function selectCurrentSpan(span)
	{
		if(commitPendingValues())
			updateDateIndicator(); 	
		unselectCurrentSpan();		
		currentSpan = span; 
		currentSpan.style.backgroundColor = "highlight"; 
		currentSpan.style.color = "highlighttext";
		currentInput = retrieveInputType(); 
		bufferLength = retrieveDateBufferLength(); 
		characterType = retrieveCharacterType(); 
		bufferMonthString = bufferString; 
	}
	function onClickDateInput(span, input)
	{		
		selectCurrentSpan(span);		
	}
	function setSelectedDate(date)
	{
		selectedDate = date;		
		this.Value = date;
		FireEvent("ValueChanged", [this]); 
		updateStatus();
		if(autoPB)
			doPostBack("ValueChanged"); 
	}
	function addOffsetToSelectedDate(offset)
	{
		if(commitPendingValues())
			return; 		
		if(selectedDate == null)
		{
			if(bindableDate != null)
				selectedDate = bindableDate;
		}	
		if(selectedDate == null)
			return;
		var day, month, year; 
		day = selectedDate.getDate(); 
		month = selectedDate.getMonth(); 
		year = selectedDate.getFullYear(); 
		if(currentInput == 3)
			year += (dateincrements[2] * offset); 
		else if(currentInput == 2)
		{
			var amonth = month + (dateincrements[1] * offset);
			if(amonth < 0)
				amonth = (12 + month) - Math.abs(dateincrements[1] * offset); 
			else if(amonth > 11)
				amonth = (month - 12) + (dateincrements[1] * offset); 
			month = amonth; 
		}
		else
		{
			var aday = day + (dateincrements[0] * offset); 
			var daysinmonth = getDaysInMonth(month, year); 
			if(aday < 1)
				aday = (daysinmonth + day) - Math.abs(dateincrements[0] * offset); 
			else if(aday > daysinmonth)
				aday = (day - daysinmonth) + (dateincrements[0] * offset); 
			day = aday; 
		}
		var d = new Date(year, month, day);
		if(!isInvalidRange(d))			
			setSelectedDate(new Date(year, month, day));
	}
	function retrieveInputType()
	{		
		if(currentSpan == null)
			return -1;			
		var spanid = currentSpan.id;
		var format = spanid.substr(id.length+1);
		if(format.indexOf("d") >= 0)
			return 1; 
		else if(format.indexOf("M") >= 0)
			return 2; 
		else if(format.indexOf("y") >= 0)
			return 3;					
		else
			return -1; 
	}
	function retrieveCharacterType()
	{
		if(currentSpan == null)
			return -1; 			
		var spanid = currentSpan.id;
		var format = spanid.substr(id.length+1); 
		if(format.indexOf("d") >= 0)
		{
			if(format.length <= 2)
				return 1; 
			else
				return 2;
		}
		else if(format.indexOf("M") >= 0)
		{
			if(format.length <= 2)
				return 1;
			else
				return 2;  
		}
		else if(format.indexOf("y") >= 0)
			return 1; 
		else
			return -1; 
	}
	function retrieveMaxLengthFromArray(array)
	{
		var max = 0; 
		for(var i=0;i<array.length;i++)
		{
			if(array[i].length > max)
				max = array[i].length; 
		}
		return max; 
	}
	function retrieveDateBufferLength()
	{
		if(currentSpan == null)
			return -1; 
			
		var spanid = currentSpan.id; 
		var format = spanid.substr(id.length + 1);
		if(format.indexOf("d") >= 0)
		{
			if(format == "d" || format == "dd")
				return 2; 
			else if(format == "ddd")
				return retrieveMaxLengthFromArray(namesArray[3]); 
			else if(format == "dddd")
				return retrieveMaxLengthFromArray(namesArray[2]); 
		}
		else if(format.indexOf("M") >= 0)
			return format.length; 
		else if(format.indexOf("y") >= 0)
			return format.length; 
	}
	function isValidPendingDate()
	{
		var d = null;
		if(dayToCommit != -1)
			d = new Date(selectedDate.getFullYear(), selectedDate.getMonth(), dayToCommit);
		else if(monthToCommit != -1)
			d = new Date(selectedDate.getFullYear(), monthToCommit, selectedDate.getDate());
		else if(yearToCommit != -1)
			d = new Date(yearToCommit, selectedDate.getMonth(), selectedDate.getDate());
		if(d != null)
		{
			if(isInvalidRange(d))
				return -1;
			else
				return 1;			
		}
		return 0;
	}	
	function commitPendingValues()
	{
		var update = false;
		if(dayToCommit != -1)
		{			
			setSelectedDate(new Date(selectedDate.getFullYear(), selectedDate.getMonth(), dayToCommit));			
			update = true;
			dayToCommit = -1; 
		}
		else if(dayToCommit == -1 && bufferString.length > 0 && currentInput == 1)
			update = true; 
			
		if(monthToCommit != -1)
		{			
			setSelectedDate(new Date(selectedDate.getFullYear(), monthToCommit, selectedDate.getDate())); 
			update = true;
			monthToCommit = -1;
		}			
		if(yearToCommit != -1)
		{
			setSelectedDate(new Date(yearToCommit, selectedDate.getMonth(), selectedDate.getDate())); 
			update = true;
			yearToCommit = -1;
		}
		else if(yearToCommit == -1 && bufferString.length > 0 && currentInput == 3)
			update = true; 
			
		bufferString = ""; 
		return update;			
	}
	function gotoNextInput(td)
	{		
		var update = commitPendingValues(); 
		var spans = td.getElementsByTagName("SPAN"); 
		var spanToSelect = null; 
		for(var i=0;i<spans.length && spanToSelect == null;i++)
		{
			if(spans[i] == currentSpan)
				spanToSelect = i +1; 
		}
		if(spanToSelect == null || spanToSelect >= spans.length)
			selectCurrentSpan(spans[0]);
		else
			selectCurrentSpan(spans[spanToSelect]); 			
			
		currentInput = retrieveInputType(); 
		return update; 
	}
	function gotoPreviousInput(td)
	{
		var update = commitPendingValues(); 
		var spans = td.getElementsByTagName("SPAN"); 		
		var spanToSelect = null; 
		for(var i=0;i<spans.length && spanToSelect == null;i++)
		{
			if(spans[i] == currentSpan)
				spanToSelect = i -1; 
		}
		if(spanToSelect == null || spanToSelect < 0)
			selectCurrentSpan(spans[spans.length-1]);
		else
			selectCurrentSpan(spans[spanToSelect]);
			
		return update;
	}
	function isValidDayBuffer(buffer)
	{
		if(buffer == "00")
			return -1;
			
		var i = parseInt(buffer, 10); 
		if(i < getDaysInMonth() && i > 0)
			return i;
		else
			return -1;
	}
	function isValidYearBuffer(buffer)
	{
		if(buffer == "00")
			return -1;
			
		if(buffer.length < 4 || buffer.substr(0,1) == "0")
			buffer = "" + (parseInt(buffer,10)-0+2000);
			
		return parseInt(buffer, 10); 
	}	
	function keyDownMonthChar()
	{
		var tmpChar = String.fromCharCode(window.event.keyCode); 		
		var tmpBuffer = bufferString + tmpChar; 
		tmpBuffer = tmpBuffer.toUpperCase(); 
		var names = null;
		if(bufferLength ==  4)
			names = namesArray[0]; 
		else
			names = namesArray[1]; 
			
		var start = numericValue;
		if(start < 0)
			start = 0;
			
		for(var i=start;i<names.length;i++)
		{
			if(names[i].toUpperCase().indexOf(tmpBuffer) == 0)
			{
				numericValue = i+1;
				monthToCommit = i; 
				bufferString = tmpBuffer; 
				return true;		
			}
		}
		for(var i=0;i<start;i++)
		{
			if(names[i].toUpperCase().indexOf(tmpBuffer) == 0)
			{
				numericValue = i+1;
				monthToCommit = i;
				bufferString = tmpBuffer; 
				return true; 
			}
		}
		if(bufferString == "")
			return false;
		else
		{
			bufferString = "";
			return keyDownMonthChar(); 
		}
	}
	function keyDownInput()
	{
		var result = -1;
		if(window.event.keyCode >= 48 && window.event.keyCode <= 57)
		{
			if(characterType == 1 && (currentInput == 1 || currentInput == 3))
			{				
				var tmpChar = String.fromCharCode(window.event.keyCode); 
				var tmpBuffer = bufferString;
				if(tmpBuffer.length < bufferLength)
				{						
					var i = -1; 
					if(currentInput == 1)
						i = isValidDayBuffer(tmpBuffer+tmpChar); 						
					else if(currentInput == 3)
						i = isValidYearBuffer(tmpBuffer+tmpChar); 						
					if(i > -1)
					{
						if(currentInput == 1)
							dayToCommit = i;
						else if(currentInput == 3)
							yearToCommit = i;
							
						bufferString = tmpBuffer + tmpChar; 
					}
					else
					{
						bufferString = tmpChar; 
						if(currentInput == 1)
							dayToCommit = isValidDayBuffer(bufferString); 
						else if(currentInput == 3)
							yearToCommit = isValidYearBuffer(bufferString); 
					}
													
					if(i>-1&&bufferString.length == bufferLength)
					{
						bufferString = ""; 								
						result = 1;
					}						
				}
				else
				{
					bufferString = tmpChar;
					var i = -1;
					if(currentInput == 1)
						i = isValidDayBuffer(bufferString); 
					else if(currentInput == 3)
						i = isValidYearBuffer(bufferString); 
						
					if(i > -1)
					{
						if(currentInput == 1)
							dayToCommit = i; 
						else if(currentInput == 3)
							yearToCommit = i; 
					}
				}
				var r = isValidPendingDate(); 
				if(r == 1)
					currentSpan.innerHTML = bufferString; 
				else if(r == -1)
					dayToCommit = yearToCommit = monthToCommit = -1;
			}
			else if(currentInput == 2)
			{
				var tmpChar = String.fromCharCode(window.event.keyCode);
				var tmpBuffer = bufferMonthString; 
				if((tmpBuffer + tmpChar).length > 2)
					tmpBuffer = tmpChar;		
				else
					tmpBuffer = tmpBuffer + tmpChar;					
				
				var i = parseInt(tmpBuffer, 10); 
				if(i >= 1 && i <= 12)
				{
					monthToCommit = i - 1;
					result = 2;
					bufferMonthString = tmpBuffer;
				}
				else
				{
					i = parseInt(tmpChar, 10); 
					if(i > 0)
					{
						monthToCommit = i -1;
						result = 2; 
						bufferMonthString = tmpChar; 
					}
				}
				var r = isValidPendingDate();
				if(r == -1)
				{
					bufferMonthString = tmpBuffer;
					monthToCommit = -1;
				}
			}			
		}
		else if(characterType == 2)
		{
			if(currentInput == 2)
			{	
				if(keyDownMonthChar())
				{
					var d = new Date(selectedDate.getFullYear(), monthToCommit, selectedDate.getDate());
					if(isInvalidRange(d))
					{
						monthToCommit = -1;
						return; 
					}
					setSelectedDate(d);
					updateDateIndicator(); 
					if(bufferLength == 4)
					{
						if(namesArray[0][monthToCommit].length == bufferString.length)
							result = 1;						
					}
					else if(bufferLength == 3)
					{
						if(namesArray[0][monthToCommit].length == bufferString.length)
							result = 1; 
					}
				}				
			}
		}
		return result; 
	}
	function onChangeDateInput()
	{
		var input = window.event.srcElement;
		var date = ccGetDateFromFormat(input.value, dateformat, namesArray); 
		if(date != 0)
			setSelectedDate(date); 
		else
			updateDateIndicator(); 		
	}
	function onKeyDownDateInput()
	{
		if(currentSpan == null)
			return; 			
		var updateIndicator = false; 
		if(window.event.keyCode == 39 || window.event.keyCode == 32)
			updateIndicator = gotoNextInput(window.event.srcElement); 
		else if(window.event.keyCode == 37)
			updateIndicator = gotoPreviousInput(window.event.srcElement);
		else if(window.event.keyCode == 38)
		{
			addOffsetToSelectedDate(1); 			
			updateIndicator = true; 
		}
		else if(window.event.keyCode == 40)
		{
			addOffsetToSelectedDate(-1);
			updateIndicator = true; 
		}
		else
		{
			var result = keyDownInput(); 
			if(result == 1)
				updateIndicator = gotoNextInput(window.event.srcElement);
			else if(result == 2)
			{
				commitPendingValues(); 
				updateIndicator = true; 
			}
		}		
		if(updateIndicator)	
			updateDateIndicator(); 
	}
	function onClickNone(button, postback)
	{
		if(readonly)
			return; 
		if(isDropDownVisible())
		{
			FireEvent("NoneButtonClick", [calendarCombo]); 			
			setSelectedDate(null); 
			updateDateIndicator();
			hideDropDown(); 
			if(postback == 1 || autoPB)
				doPostback("NoneButtonClick"); 
		}
	}
	function onClickDay(td,month)
	{		
		if(readonly)
			return;
		if(tdSelected != td)
		{			
			if(td.getAttribute("invalid") != null && td.getAttribute("invalid") == "1")
			{
				hideDropDown(); 
				return; 
			}				
			var date = td.getAttribute("day"); 
			if(typeof(date) == "string")
				date = new Date(date);
			var monthDiff = isMonthEqual(date, monthDates[month]); 			
			if(monthDiff != 0)
			{			
				if(!isDropDownVisible())
				{
					selectedDate = date; 
					monthDates = getMonthDates(addMonth(monthDates[0], monthDiff));
					updateCalendars(monthDates, id, columns, rows);
					td = getCellWithDate(date); 					
				}
			}						
			selectedDate = date; 
			td.className = getCssForDay(date,month);
			if(tdSelected != null)
			{				
				if(monthDiff == 0)
				{
					var ds = tdSelected.getAttribute("day"); 
					if(typeof(ds) == "string")
						ds = new Date(ds); 
					tdSelected.className = getCssForDay(ds, month, true); 
				}
			}
			tdSelected = td; 
			setSelectedDate(date);
		}		
		if(isDropDownVisible())
		{
			updateDateIndicator(); 		
			hideDropDown(); 			
		}
	}
	function updateDateIndicator()
	{
		if(maskedFormats == null)
		{
			var element = document.getElementById(id + "_dateindicator"); 
			if(element == null)
				throw new Error("invalid operation exception");
				
			var input = element.getElementsByTagName("INPUT"); 
			if(input != null && input.length == 1)
			{
				if(selectedDate == null)
					input[0].value = ""; 
				else
					input[0].value = ccFormatDate(selectedDate, dateformat, namesArray); 				
			}
			else	
			{
				if(selectedDate == null)
					element.innerHTML = "&nbsp;"; 
				else			
					element.innerHTML = ccFormatDate(selectedDate, dateformat, namesArray); 		
			}
		}
		else
		{
			var formats = new Array("d", "dd", "ddd", "dddd", "M", "MM", "MMM", "MMMM", "y", "yy", "yyyy"); 
			for(var i=0;i<formats.length;i++)
			{
				var format = formats[i]; 
				var maskedarray = maskedFormats[format]; 
				if(maskedarray != null)
				{
					var b = false;
					var s = ""; 
					if(selectedDate == null)
					{
						selectedDate = bindableDate;
						b = true; 
					}
					for(var j=0;j<maskedarray.length;j++)
					{
						var span = maskedarray[j];
						var s = "";
						if(format == "d")
							s = selectedDate.getDate(); 
						else if(format == "dd")
							s = LZ(selectedDate.getDate()); 
						else if(format == "ddd")
							s = namesArray[3][selectedDate.getDay()]; 
						else if(format == "dddd")
							s = namesArray[2][selectedDate.getDay()];
						else if(format == "M")
							s = selectedDate.getMonth() + 1; 
						else if(format == "MM")
							s = LZ(selectedDate.getMonth()+1); 
						else if(format == "MMM")
							s = namesArray[1][selecteDate.getMonth()];
						else if(format == "MMMM")
							s = namesArray[0][selectedDate.getMonth()];
						else if(format == "y" || format == "yy" || format == "yyyy")
						{
							var y = selectedDate.getFullYear() + "";
							if(y.length < 4)
								y = "" + (y-0+1900);
							if(format == "y")
								s = "" + y;
							else if(format == "yy")
								s = y.substring(2,4); 
							else if(format == "yyyy")
								s = y; 
						}
						if(typeof(s) != "string")
							s = "" + s;
						if(s.length > 0)
						{
							if(b)
							{
								var h = s.length;
								s = "";
								for(var k=0;k<h;k++)
									s += "&nbsp;";								
							}							
							span.innerHTML = s; 							
						}
					}
					if(b)
						selectedDate = null; 					
				}
			}
		}
	}
	function isDropDownVisible()
	{	
		return (dropdown != null && dropdown.style.visibility == "visible"); 
	}
	function hideDropDownEx()
	{
		if(dropdown == null)
			return; 
			
		dropdown.style.visibility = "hidden"; 
		dropdown = null; 
		if(refreshCalendar)
		{
			monthDates[0] = selectedDate; 
			updateCalendars(monthDates, id, columns, rows);
		}
		if(calendarCSS[11] != "")
			button.className = calendarCSS[11]; 			
		button.removeAttribute("pressed"); 		
		refreshCalendar = false; 
		hideBackFrame("jwcBackFrame"); 
	}
	function hideDropDown()
	{
		window.setTimeout(hideDropDownEx, 100); 		
	}	
	function onClickMonth(tr)
	{
		if(readonly)
			return;
		refreshSelectedMonth(tr); 	
		lastRow = null; 	
	}
	function refreshSelectedMonth(tr)
	{
		var date = new Date(parseInt(tr.getAttribute("iyear"), 10), parseInt(tr.getAttribute("imonth"), 10), 1); 
		if(isInvalidRange(date))
			return;
		var i = scrollableInMonth;
		while(i>0)
		{		
			date = addMonth(date, -1); 
			i--;
		}
		monthDates = getMonthDates(date); 
		updateCalendars(monthDates, id, columns, rows); 		
		FireEvent("DatesChanged", null); 
		updateStatus(); 
	}
	function onMouseOverToday(button)
	{		
		var css = calendarCSS[8];
		if(css.length > 0)
			button.className = css; 
	}
	function onMouseOutToday(button)
	{
		var css = calendarCSS[7]; 
		if(css.length > 0)
			button.className = css; 
	}
	function onMouseOverNone(button)
	{
		var css = calendarCSS[10]; 
		if(css.length > 0)
			button.className = css; 
	}
	function onMouseOutNone(button)
	{		
		var css = calendarCSS[9]; 
		if(css.length > 0)
			button.className = css; 
	}
	function onMouseOverMonth(tr)
	{
		if(lastRow != null && lastRow.className != calendarCSS[4])
			lastRow.className = calendarCSS[4]; 
			
		tr.className = calendarCSS[5]; 
		lastRow = tr; 
	}
	function onMouseOutMonth(tr)
	{
		tr.className = calendarCSS[4]; 
	}
	var lastClientY = null; 
	function onMouseMoveScrollableMonths()
	{
		lastClientY = window.event.clientY; 				
	}
	var intervalID = null; 
	function onMouseUpScrollableMonths(td)
	{
		if(intervalID != null)
		{
			window.clearInterval(intervalID); 
			if(window.event.srcElement != null && lastRow != null)
			{
				if(window.event.srcElement == lastRow || lastRow.contains(window.event.srcElement))
				{
					refreshSelectedMonth(lastRow);
					lastRow = null; 
				}						
			}						
			HideScrollableMonths(); 						
			intervalID = null; 
			if(isDropDownVisible())
			{
				if(dropdown.focus != null)
					dropdown.focus(); 
					
				if(browser.isNetscape)					
					document.activeElement = dropdown;
			}
		}		
	}
	function onLoad()
	{
		FireEvent("Load", [this]); 
		if(showAsDropDown)
		{			
			var e = document.getElementById(id+"_dropdown"); 
			if(e != null)
			{
				if(e.parentElement.tagName != "BODY")
					document.body.appendChild(e); 
			}
			e = document.getElementById(id+"_months"); 
			if(e != null)
			{
				if(e.parentElement.tagName != "BODY")
					document.body.appendChild(e); 
			}
		}
		
	}
	function getDatesArray(dates, cols, rows)
	{
		var array = null; 
		if(cols == null && rows == null)
		{
			if(dates != null)
			{
				array = new Array(dates.length / 3); 
				var j = 0; 
				for(var i=0;i<dates.length;i=i+3)
				{
					array[j] = new Date(dates[i], dates[i+1], dates[i+2]); 
					j++;
				}
			}
		}
		else
		{
			array = new Array(cols*rows); 
			var j = 0; 
			for(var i=0;i<dates.length;i=i+2)
			{
				array[j] = new Date(dates[i], dates[i+1], 1); 
				j++; 			
			}
		}
		return array; 		
	}
	function FireEvent(name, params)
	{
		if(events == null || events.length == 0)
			return null;
			
		var l = events.length; 
		var handler = "";		
		for(var i=0; i<l; i=i+2)
		{
			if(events[i] == name) 
			{
				handler = events[i+1];
				i = l; 
			}
		}
		if(handler != "")
		{	
			var _params = "";
			var l = (params == null) ? 0 : params.length; 
			for(var i=0;i<l; i++)
			{
				if(_params != "")
					 _params += ",";
				_params += "params[" + i + "]";
			}		
			var _cmd = "return eval(" + handler + "("  + _params + "))";
			var _function = new Function("params", _cmd);
			return _function(params);
		}
		return null; 
	}
	function addMonth(date, offset)
	{
		if(date.getMonth() == 11 && offset == 1)
			return new Date(date.getFullYear() + 1, 0, 1);
		else if(date.getMonth() == 0 && offset == -1)
			return new Date(date.getFullYear() - 1, 11, 1); 
		else
			return new Date(date.getFullYear(), date.getMonth() + offset, 1); 
	}
	function getMonthDates(startDate)
	{
		var array = new Array(columns*rows);		
		for(var i=0;i<array.length;i++)
		{
			array[i] = startDate; 
			startDate = addMonth(startDate, 1);
		}
		return array; 
	}
	function getDaysInMonth(month, year)
	{
		if(month == null)
			month = selectedDate.getMonth(); 
		if(year == null)
			year = selectedDate.getFullYear();
			
		var days = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31); 
		if(month == 1)
		{
			if((year % 4) == 0)
			{
				if(((year % 100) != 0) || ((year % 400) != 0))
					days[1] = 29; 
			}
		}
		return days[month]; 
	}
	function getCalendarDays(date, firstDayWeek,includePreviousMonth,includeNextMonth)
	{
		var days = new Array(42);
		var previousMonth = -1;
		var previousYear = -1;
		var nextMonth = -1; 
		var nextYear = -1; 
		var lastPreviousMonthDay = -1; 
		var nextFirstMonthDay = 1; 
		var firstDay = (new Date(date.getFullYear(), date.getMonth(), 1)).getDay(); 		
		firstDay = ((firstDay - firstDayWeek) < 0) ? ((firstDay - firstDayWeek) + 7) : (firstDay - firstDayWeek);
		if(firstDay != 0)
		{
			if(date.getMonth() == 0)
			{
				lastPreviousMonthDay = getDaysInMonth(11, date.getFullYear() - 1); 
				previousMonth = 11; 
				previousYear = date.getFullYear() -1; 
			}
			else
			{
				lastPreviousMonthDay = getDaysInMonth(date.getMonth()-1, date.getFullYear()); 
				previousMonth = date.getMonth() - 1;
				previousYear = date.getFullYear(); 
			}
		}
		if(date.getMonth() == 11)
		{
			nextYear = date.getFullYear() + 1; 
			nextMonth = 0; 
		}
		else
		{
			nextYear = date.getFullYear(); 
			nextMonth = date.getMonth() + 1; 
		}
		var daysFilled = 0; 
		var iday = firstDay; 
		if(lastPreviousMonthDay != -1)
		{
			var lastiday = iday - 1; 
			while(lastiday >= 0)
			{
				var pair = new Pair();
				pair.First = new Date(previousYear, previousMonth, lastPreviousMonthDay); 
				pair.Second = false; 			
				if(includePreviousMonth)
					pair.Second = true;
				days[lastiday] = pair;
				lastiday--;
				lastPreviousMonthDay--;
				daysFilled++;			
			}
		}
		var j = 0;
		var day = 1;
		var lastDay = getDaysInMonth(date.getMonth(), date.getFullYear()); 
		while(day <= lastDay)
		{
			if((iday != 0) && ((iday % 7) == 0))
			{
				j++;
				iday = 0; 
			}
			var pair = new Pair();
			pair.First = new Date(date.getFullYear(), date.getMonth(), day); 
			pair.Second = true;
			days[(j*7)+iday] = pair; 
			day++;
			iday++;
			daysFilled++;			
		}
		while(daysFilled < 42)
		{			
			if((iday % 7) == 0)
			{
				j++;
				iday = 0; 
			}
			var pair = new Pair();
			pair.First = new Date(nextYear, nextMonth, nextFirstMonthDay); 
			pair.Second = false;
			if(includeNextMonth)
				pair.Second = true; 
			days[(j*7)+iday] = pair;
			iday++;
			daysFilled++;
			nextFirstMonthDay++; 
		}
		return days;
	}
	function getPairForWeek(days, i, j)
	{
		var pairA = days[i];
		var pairB = days[j];
		var dateA = pairA.First;
		var dateB = pairB.First; 		
		if(pairB.Second && dateB > dateA)
			return pairB;
		else
			return pairA; 		
	}	
	var daymsec = 1000*60*60*24;
	function getFirstDayWeek(adate, firstdayweek)
	{
		var date = new Date()
		for (var i=0; i<=7; i++)
		{	
			date.setTime(adate.getTime()-(daymsec*i));				
			if(date.getDay() ==  firstdayweek)
				return date;
		}
		return adate;
	}	
	function getWeekNumber(adate, firstdayweek, weekrule)
	{		
		var date = new Date()
		date.setTime(adate.getTime())
		date = getFirstDayWeek(date, firstdayweek)
		for(var i=0; i<53; i++)
		{					
			if(date.getFullYear() != adate.getFullYear() || (date.getMonth() == 0 && date.getDay() == firstdayweek && date.getDate() == 1))
				return i+1;
				
			date.setTime(date.getTime()-(daymsec*7))
		}
	}
	function isSelectedDate(day)
	{	
		if(selectedDate != null)
		{
			if(selectedDate.getDate() == day.getDate() && selectedDate.getMonth() == day.getMonth() && selectedDate.getFullYear() == day.getFullYear())
				return true; 
		}
		return false; 
	}
	function isToday(day)
	{
		var date = new Date();
		return (date.getDate() == day.getDate() && date.getMonth() == day.getMonth() && date.getFullYear() == day.getFullYear()); 
	}
	function isInvalidDate(day, indexMonth)
	{
		for(var i=0;i<monthDates.length;i++)
		{
			var d = monthDates[i]; 
			if(d.getMonth() == day.getMonth() && d.getFullYear() == day.getFullYear())
				return false;
		}
		return true; 
	}
	function isInvalidRange(day)
	{
		if(day >= minDate && day <= maxDate)
			return false;
		else
			return true; 
	}
	function isBoldedDate(day)
	{
		if((boldedDates == null || boldedDates.length == 0) && (anuallyBoldedDates == null || anuallyBoldedDates.length == 0))
			return false; 
			
		if(boldedDates != null)
		{
			for(var i=0;i<boldedDates.length;i++)
			{
				var date = boldedDates[i];
				if((day.getDate() == date.getDate()) && (day.getMonth() == date.getMonth()) && (day.getFullYear() == date.getFullYear()))
					return true; 
			}
		}
		if(anuallyBoldedDates != null)
		{		
			for(var i=0;i<anuallyBoldedDates.length;i++)
			{
				var date = anuallyBoldedDates[i]; 
				if((day.getDate() == date.getDate()) && (day.getMonth() == date.getMonth()))
					return true; 
			}
		}
		return false; 
	}
	function getCssForDay(day, indexMonth, forceToday)
	{
		var css = calendarCSS[0];
		var isInvalid = isInvalidDate(day, indexMonth) || isInvalidRange(day); 		
		if(isSelectedDate(day))
		{
			if(!isInvalid)
				css += " " + calendarCSS[2]; 
		}
		if(isToday(day) && (!isInvalid || forceToday)) 
			css += " " + calendarCSS[3]; 		
		if(isBoldedDate(day))
			css += " " + calendarCSS[1]; 
		if(isInvalid)
			css += " " + calendarCSS[6]; 
		return css; 
	}
	function getCellWithDateCore(d, table)
	{		
		var m = table.rows[0].cells[1].getAttribute("month"); 		
		if(typeof(m) == "string")
			m = new Date(m); 
		if(m.getMonth() != d.getMonth())
			return null;
		var offsetRow = 3;
		var j = 0;
		for(var r=offsetRow;r<table.rows.length;r++)
		{
			var row = table.rows[r];
			var i = 0; 
			for(var c=weeknumber ? 2 : 1;c<row.cells.length-1;c++)
			{
				var td = row.cells[c]; 
				var da = td.getAttribute("day"); 
				if(typeof(da) == "string")
					da = new Date(da); 
				if(d.getDate() == da.getDate() && d.getMonth() == da.getMonth() && d.getFullYear() == da.getFullYear())
					return td; 
			}
		}
		return null; 
	}
	function getCellWithDate(d)
	{
		var div = document.getElementById(id+"_dropdown"); 
		if(div == null)
			div = document.getElementById(id);
		var table = div.getElementsByTagName("TABLE")[0]; 
		if(columns ==  1 && rows == 1)
			return getCellWithDateCore(d, table); 
		else
		{
			for(var j=0;j<rows;j++)
			{
				var row = table.rows[j];
				for(var i=0;i<columns;i++)
				{
					var cell = row.cells[i]; 
					var td = getCellWithDateCore(d, cell.getElementsByTagName("TABLE")[0]); 
					if(td != null)
						return td; 
				}				
			}
		}
		return null; 
	}	
	function updateCalendar(monthDate, table, indexMonth, includePreviousMonth, includeNextMonth)
	{
		table.rows[0].cells[1].setAttribute("month", monthDate); 
		table.rows[0].cells[1].innerHTML = monthnames[monthDate.getMonth()] + " " + monthDate.getFullYear();				
		var days = getCalendarDays(monthDate, firstdayweek, includePreviousMonth, includeNextMonth);
		var offsetRow = 3; 
		var j = 0; 		
		for(var r=offsetRow;r<table.rows.length;r++)
		{
			var row = table.rows[r];
			var i = 0; 
			if(weeknumber)
			{
				var pairweek = getPairForWeek(days,  j*7, (j*7)+6);				
				if(pairweek.Second)
					row.cells[0].innerHTML = getWeekNumber(pairweek.First, firstdayweek, weekrule); 
				else
					row.cells[0].innerHTML = "&nbsp;"; 
			}
			for(var c=weeknumber ? 2 : 1;c<row.cells.length-1;c++)
			{
				var css = ""; 
				var pair = days[(j*7)+i];
				var cell = row.cells[c];
				if(pair.Second)
				{
					var css = getCssForDay(pair.First, indexMonth); 				
					cell.className = css;					
				}
				if(isSelectedDate(pair.First))
				{
					if(pair.Second)
					{
						if(tdSelected != null && tdSelected != cell)
						{
							var d = tdSelected.getAttribute("day"); 
							if(typeof(d) == "string")
								d = new Date(d); 
							tdSelected.className = getCssForDay(d, indexMonth);
						}							
						tdSelected = cell; 
						tdSelected.className = getCssForDay(pair.First, indexMonth); 
					}
					else						
						cell.className = calendarCSS[0]; 
				}
				cell.setAttribute("day", pair.First);
				cell.setAttribute("invalid", isInvalidRange(pair.First) ? "1" : "0");
				if(pair.Second)
					cell.innerHTML = pair.First.getDate(); 
				else
					cell.innerHTML = "&nbsp;";
					
				i++;
			}
			j++; 
		}
	}
	function updateCalendars(monthDates, id, columns, rows)
	{
		var div = document.getElementById(id + "_dropdown"); 
		if(div == null)
			div = document.getElementById(id); 
		var table = div.getElementsByTagName("TABLE")[0]; 
		if(columns == 1 && rows == 1)
			updateCalendar(monthDates[0], table, 0, true, true);
		else
		{			
			for(var j=0;j<rows;j++)
			{
				var row = table.rows[j]; 
				for(var i=0;i<columns;i++)
				{
					var cell = row.cells[i]; 
					updateCalendar(monthDates[(j*columns)+i], cell.getElementsByTagName("TABLE")[0], (j*columns)+i, ((j*columns)+i) == 0, ((j*columns)+i) == monthDates.length - 1); 
				}
			}
		}		
	}
	var dropdown = null; 
	var button = null; 
	function onMouseOverDropDown(td)
	{		
		if(td.getAttribute("pressed") != null)
		{
			if(calendarCSS[13] != "")
				td.className = calendarCSS[13]; 
		}
		else if(calendarCSS[12] != "")
			td.className = calendarCSS[12]; 
	}
	function onMouseOutDropDown(td)
	{			
		if(td.getAttribute("pressed") != null)
		{
			if(calendarCSS[13] != "")
				td.className = calendarCSS[13]; 
		}
		else if(calendarCSS[11] != "")
			td.className = calendarCSS[11]; 
	}
	function ShowDropDown(td, x, y)
	{
		if(isDropDownVisible())		
		{
			hideDropDown(); 			
			return; 
		}
		if(button == null)
			button = td;
		button.setAttribute("pressed", "1");
		if(calendarCSS[13] != "")
			button.className = calendarCSS[13];
		var element = document.getElementById(id + "_dropdown"); 
		if(element == null)
			throw new Error("invalid operation exception");
		dropdown = element; 				
		monthDates = getMonthDates(selectedDate == null ? new Date() : selectedDate); 
		updateCalendars(monthDates, id, columns, rows); 				
		var left = getPixelLeft(td) + td.offsetWidth - element.offsetWidth; 
		if(left < 0)
		{
			left = 0; 
			left += getPixelValue(document.body.currentStyle.paddingLeft);
			left += getPixelValue(document.body.currentStyle.marginLeft);			
		}
		element.style.left = left + "px"; 
		var top = getPixelTop(td) + td.offsetHeight; 
		element.style.top = top + "px"; 
		element.style.display = ""; 
		element.style.visibility = "visible"; 			
		showBackFrame("jwcBackFrame",element.style.pixelLeft,element.style.pixelTop,element.offsetWidth,element.offsetHeight); 
		if(element.focus != null)
			element.focus();		
		else if(element.onfocus != null)
			element.onfocus();			
		if(browser.isNetscape)				
			document.activeElement = element;
	}
	function ShowPreviousMonth()
	{
		if(readonly)
			return; 
		var d = addMonth(monthDates[0], -1); 
		if(isInvalidRange(d))
		{
			if(minDate >= d && minDate.getMonth() == d.getMonth() && minDate.getFullYear() == d.getFullYear())
			{ }
			else
				return;
		}			
		monthDates = getMonthDates(d); 
		updateCalendars(monthDates, id, this.Columns, this.Rows); 		
		FireEvent("DatesChanged", null); 		
		updateStatus(); 
		if(dropdown != null && dropdown.focus != null)
			dropdown.focus(); 
	}
	var monthsScrollable = null; 
	function scrollMonths()
	{
		if(lastClientY == null)
			return; 
		var scrollDirection = 0; 
		if(lastClientY < getPixelTop(monthsScrollable))
			scrollDirection = -1;
		else if(lastClientY > (getPixelTop(monthsScrollable) + monthsScrollable.offsetHeight))
			scrollDirection = 1; 			
		if(scrollDirection != 0)
		{
			var scrollableTable = monthsScrollable.getElementsByTagName("TABLE")[0]; 
			var row = null;
			var imonth = -1;
			var iyear = -1; 
			var cancel = false;			
			for(var i=0;i<scrollableTable.rows.length&&!cancel;i++)
			{
				row = scrollableTable.rows[i]; 
				imonth = parseInt(row.getAttribute("imonth"), 10); 
				iyear = parseInt(row.getAttribute("iyear"), 10); 
				if(scrollDirection == -1)
				{
					if(imonth - 1 < 0)
					{
						imonth = 11;
						iyear--;
					}
					else
						imonth--; 
				}
				else
				{
					if(imonth + 1 > 11)
					{
						imonth = 0; 
						iyear++; 
					}
					else
						imonth++; 
				}
				var d = new Date(iyear, imonth, 1);
				if(!isInvalidRange(d))
				{					
					row.setAttribute("imonth", imonth);
					row.setAttribute("iyear", iyear); 
					row.cells[0].innerHTML = monthnames[imonth] + "&nbsp;" + iyear; 						
				}
				else
				{
					cancel = true; 			
					if(intervalID != null)
						window.clearInterval(intervalID); 
				}
			}			
		}			
	}
	var lastRow = null;
	function RefreshScrollableMonths(month)
	{		
		var scrollableTable = monthsScrollable.getElementsByTagName("TABLE")[0];
		var row = scrollableTable.rows[3]; 
		row.setAttribute("imonth", month.getMonth()); 
		row.setAttribute("iyear", month.getFullYear()); 
		row.cells[0].innerHTML = monthnames[month.getMonth()] + "&nbsp;" + month.getFullYear(); 
		row.className = calendarCSS[5];
		lastRow = row; 
		var icount = 2;
		var imonth = month.getMonth(); 
		var iyear = month.getFullYear(); 
		while(icount >= 0)
		{
			if(imonth - 1 < 0)
			{
				imonth = 11;
				iyear--;
			}
			else
				imonth--;
				
			row = scrollableTable.rows[icount]; 
			row.setAttribute("imonth", imonth); 
			row.setAttribute("iyear", iyear); 
			row.cells[0].innerHTML = monthnames[imonth] + "&nbsp;" + iyear;
			icount--; 
		}
		icount = 4;
		imonth = month.getMonth(); 
		iyear = month.getFullYear(); 
		while(icount <= 6)
		{
			if(imonth + 1 > 11)
			{
				imonth = 0;
				iyear++;
			}
			else
				imonth++;
				
			row = scrollableTable.rows[icount]; 
			row.setAttribute("imonth", imonth); 
			row.setAttribute("iyear", iyear); 
			row.cells[0].innerHTML = monthnames[imonth] + "&nbsp;" + iyear;
			icount++; 
		}
	}
	function HideScrollableMonths()
	{
		monthsScrollable.style.visibility = "hidden"; 
		scrollableInMonth = -1;
	}
	var scrollableInMonth = -1; 
	function ShowScrollableMonths(td)	
	{
		if(readonly)
			return;
		if(monthsScrollable == null)
			monthsScrollable = document.getElementById(id + "_months"); 		
		if(intervalID == null)
			intervalID = window.setInterval(scrollMonths, 250);					
		var d = td.getAttribute("month"); 
		if(typeof(d) == "string")
			d = new Date(d); 
		RefreshScrollableMonths(d);
		monthsScrollable.style.visibility = "visible"; 		
		monthsScrollable.style.top = (getPixelTop(td) - (monthsScrollable.offsetHeight / 2)) + "px"; 
		monthsScrollable.style.left = getPixelLeft(td) + "px"; 
		monthsScrollable.style.width = td.offsetWidth;		
		scrollableInMonth = parseInt(td.getAttribute("monthindex"), 10); 
	}
	function ShowNextMonth()
	{
		if(readonly)
			return;
		var d = addMonth(monthDates[0], 1); 
		if(isInvalidRange(d))
		{
			if(maxDate >=  d && maxDate.getMonth() == d.getMonth() && maxDate.getFullYear() == d.getFullYear())
			{ }
			else
				return;
		}
		monthDates = getMonthDates(d); 
		updateCalendars(monthDates, id, this.Columns, this.Rows);
		FireEvent("DatesChanged", null); 
		updateStatus(); 
		if(dropdown != null && dropdown.focus != null)
			dropdown.focus(); 
	}
	function initializeCalendar(daysTable,g )
	{
		daysTable.rows[0].cells[1].setAttribute("month", monthDates[g]);
		daysTable.rows[0].cells[1].setAttribute("monthindex", g);
		var offsetRow = 3;		
		var firstDay = false; 
		var useMonth = null;
		for(var r=offsetRow;r<daysTable.rows.length;r++)
		{
			var rowDay = daysTable.rows[r]; 
			for(var c=weeknumber ? 2 : 1;c<rowDay.cells.length-1;c++)
			{
				try
				{								
					var day = parseInt(rowDay.cells[c].innerHTML, 10);
					var date = null; 								
					if(day > 1 && !firstDay)
						useMonth = addMonth(monthDates[g], -1); 
					else if(day == 1 && !firstDay)
					{
						firstDay = true;
						useMonth = monthDates[g]; 
					}
					else if(day == 1 && firstDay)
						useMonth = addMonth(monthDates[g], 1); 
										
					var date = new Date(monthDates[g].getFullYear(), useMonth.getMonth(), day);
					rowDay.cells[c].setAttribute("day", date); 
					if(isSelectedDate(date))
						tdSelected = rowDay.cells[c]; 
				}
				catch(e) { }
			}
		}
	}
	function initializeCalendars()
	{
		var g = 0; 
		var div = document.getElementById(id+"_dropdown"); 		
		if(div == null)
			div = document.getElementById(id);		
		if(div.getElementsByTagName("TABLE").length > 0)
		{
			var table = div.getElementsByTagName("TABLE")[0]; 
			if(rows == 1 && columns == 1)
				initializeCalendar(table, g); 
			else
			{
				for(var j=0;j<rows;j++)
				{				
					var row = table.rows[j]; 
					for(var i=0;i<columns;i++)
					{
						var cell = row.cells[i]; 						 
						initializeCalendar(cell.getElementsByTagName("TABLE")[0], g); 
						g++;						
					}
				}
			}
		}
		else		
			throw new Error("not implemented exception");
	}
	initializeCalendars();
	if(browser.isNetscape)
	{
		window.addEventListener("load", function() { calendarComboOnLoad(id); }, false );
		window.addEventListener("mousemove", function() { mouseMoveScrollableMonths(id); }, false); 
		window.addEventListener("mouseup", function() { mouseUpScrollableMonths(id); }, false); 
		window.addEventListener("keydown", function() { onKeyDownDateInput(id); }, false); 
	}
	else
	{
		window.attachEvent("onload", function() { calendarComboOnLoad(id); }); 
		document.body.attachEvent("onmousemove", function() { mouseMoveScrollableMonths(id); } ); 
		document.body.attachEvent("onmouseup", function() { mouseUpScrollableMonths(id); }); 		
	}
	var element = document.getElementById(id + "_dropdown"); 
	if(element != null)	
	{
		if(browser.isIE)				
			element.attachEvent("onblur", function() { calendarComboOnBlur(id); }); 
	}	
	var maskedFormats = null; 
	element = document.getElementById(id + "_dateindicator");
	if(element != null)
	{
		var spans = element.getElementsByTagName("SPAN");
		if(spans != null && spans.length > 0)
		{
			for(var i=0;i<spans.length;i++)
			{
				var spanid = spans[i].id;
				var format = spanid.substr(id.length+1);
				if(maskedFormats == null)
					maskedFormats = new Array(); 
				var spanarray = maskedFormats[format];
				if(spanarray == null)
					spanarray = new Array(); 
				spanarray[spanarray.length] = spans[i]; 
				maskedFormats[format] = spanarray;
			}
		}		
	}
	var calendarCombo = this; 
	return this; 
}