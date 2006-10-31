using System;
using System.Data;
using Janus.Web.GridEX;

namespace YP2006
{
	/// <summary>
	/// Summary description for GridFunctionsClass.
	/// </summary>
	public class GridFunctionsClass
	{
		public GridFunctionsClass()
		{

		}

		static public void SetDateGrid(string szDate, DataSet dsDateDataSet, GridEX grdDateGrid)
		{
			DataRow drDateDataRow;
			if(szDate != null && szDate != "")
			{
				if(dsDateDataSet.Tables.Count > 0)
				{
					drDateDataRow = dsDateDataSet.Tables[0].NewRow();
					if(dsDateDataSet.Tables[0].Columns.Count > 0)
					{
						drDateDataRow[0] = DateTime.ParseExact(szDate, Global.szDatabaseDateFormat, null);
						dsDateDataSet.Tables[0].Rows.Add(drDateDataRow);
					}
				}
			}
			grdDateGrid.DataBind();
		}


	}//END OF CLASS
}//END OF NAMESAPCE
