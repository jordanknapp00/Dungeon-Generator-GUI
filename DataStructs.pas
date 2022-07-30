unit DataStructs;

interface

uses System.Generics.Collections;

type
  TDoor = class
  public
    x: Double;
    y: Double;
    divDim: Double;

    isHorizontal: Boolean;

    other: TDoor;

  published
    constructor Create(xPos, yPos: Double; horiz: Boolean);
  end;

  TRoom = class
  public
    leftWall: Double;
    rightWall: Double;
    topWall: Double;
    bottomWall: Double;

    id: Char;

    doors: TObjectList<TDoor>;

  published
    constructor Create(left, right, bottom, top: Double);

    procedure AddDoor(toAdd: TDoor);
  end;

implementation

// -----------------------------------------------------------------------------
// TDOOR METHODS
// -----------------------------------------------------------------------------

constructor TDoor.Create(xPos, yPos: Double; horiz: Boolean);
begin
  x := xPos;
  y := yPos;
  isHorizontal := horiz;

  if isHorizontal then
    divDim := x
  else
    divDim := y;
end;

// -----------------------------------------------------------------------------
// TROOM METHODS
// -----------------------------------------------------------------------------

constructor TRoom.Create(left, right, bottom, top: Double);
begin
  leftWall := left;
  rightWall := right;
  topWall := top;
  bottomWall := bottom;

  doors := TObjectList<TDoor>.Create(true);
end;

procedure TRoom.AddDoor(toAdd: TDoor);
begin
  doors.Add(toAdd);
end;

end.
