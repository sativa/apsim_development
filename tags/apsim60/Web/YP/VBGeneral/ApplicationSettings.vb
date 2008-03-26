Imports System.Windows.Forms


' ----------------------------------------------
' Simple base class for a user interface manager
' ----------------------------------------------
Public MustInherit Class ApplicationSettings


    ' --------------------------------------------
    ' Return access to an imagelist of small icons
    ' --------------------------------------------
    MustOverride ReadOnly Property SmallImageList() As ImageList

    ' --------------------------------------------
    ' Return access to an imagelist of small icons
    ' --------------------------------------------
    MustOverride Function SmallImageIndex(ByVal ComponentType As String) As Integer

    ' -------------------------------------------------
    ' Return true if the specified component is visible
    ' to the user.
    ' -------------------------------------------------
    MustOverride Function IsComponentVisible(ByVal ComponentName As String) As Boolean

    ' -------------------------------------------------
    ' Return true if the specified component type can
    ' be added as a child to the specified parent type.
    ' -------------------------------------------------
    MustOverride Function AllowComponentAdd(ByVal ChildComponentType As String, ByVal ParentComponentType As String) As Boolean

    ' -------------------------------------
    ' Create a User interface form for the
    ' specified type.
    ' -------------------------------------
    MustOverride Function CreateUI(ByVal UIType As String) As BaseUI


End Class
