unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, SynEditHighlighter;

type

  { TfrmConfig }

  TfrmConfig = class(TForm)
    ColorElementTree: TTreeView;
    Label1: TLabel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure SynEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    function GetHighlighter: TSynCustomHighlighter;
    procedure SetHighlighter(AValue: TSynCustomHighlighter);
    { private declarations }
  public
    { public declarations }
    property CurrentHighlighter : TSynCustomHighlighter read GetHighlighter write SetHighlighter;
  end;

var
  frmConfig: TfrmConfig;

implementation
uses
  unit2;

{$R *.lfm}

{ TfrmConfig }

procedure TfrmConfig.FormCreate(Sender: TObject);
begin
  self.SynEdit1.Highlighter := Form2.SynEditMiniPas.Highlighter;
  //SynEdit1.Lines.Text := SynEdit1.Highlighter.SampleSource;;
  SynEdit1.Lines.Text := Form2.SynEditMiniPas.Text;

end;

procedure TfrmConfig.SynEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Token: String;
  Attri: TSynHighlighterAttributes;//TSynHighlightElement;
  MouseXY, XY: TPoint;
  AddAttr: TSynHighlighterAttributes;//TAdditionalHilightAttribute;
  NewNode: TTreeNode;
begin
  MouseXY := Point(X - (SynEdit1.CharWidth div 2), Y);
  XY := SynEdit1.PixelsToRowColumn(MouseXY);
  NewNode := nil;
  // Gutter Colors
  if X <= SynEdit1.Gutter.Width then begin
    {for i := 0 to SynEdit1.Gutter.Parts.Count-1 do begin
      if SynEdit1.Gutter.Parts[i].Width > X then begin
        if SynEdit1.Gutter.Parts[i] is TSynGutterLineNumber then
          SelectAhaColor(ahaLineNumber)
        else
        if SynEdit1.Gutter.Parts[i] is TSynGutterChanges then
          SelectAhaColor(ahaModifiedLine)
        else
        if SynEdit1.Gutter.Parts[i] is TSynGutterCodeFolding then
          SelectAhaColor(ahaCodeFoldingTree)
        else
          SelectAhaColor(ahaGutter);
        exit;
      end;
      X := X - SynEdit1.Gutter.Parts[i].Width;
    end;}
    exit;
  end;
  // Line Highlights
  {if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(XY.Y);
    if AddAttr = ahaFoldedCode then begin
      if not( (XY.X >= Length(SynEdit1.Lines[XY.Y-1]) + 4) and
              (XY.X <= Length(SynEdit1.Lines[XY.Y-1]) + 6) )
      then
        AddAttr := ahaNone;
        //NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[AddAttr]);
    end;
    if AddAttr <> ahaNone then begin
      SelectAhaColor(AddAttr);
      exit;
    end;
      //NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[AddAttr]);
  end;
  if (XY.Y = SynEdit1.CaretY) and
     (XY.X > Length(SynEdit1.Lines[XY.Y - 1])+1)
  then begin
    SelectAhaColor(ahaLineHighlight);
    exit;
    //NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[ahaLineHighlight]);
  end;
  if FIsEditingDefaults then
    exit;}
  // Pascal Highlights
  Token := '';
  Attri := nil;
  SynEdit1.GetHighlighterAttriAtRowCol(XY, Token, Attri);
  if Attri = nil then
    Attri := CurrentHighlighter.WhitespaceAttribute;
  if Attri <> nil then begin
    Label1.Caption:= Format('%s (%s)', [Attri.Name, Attri.StoredName]) ;
    NewNode := ColorElementTree.Items.GetFirstNode;
    {while Assigned(NewNode) do begin
      if (NewNode.Data <> nil)
      and (TColorSchemeAttribute(NewNode.Data).StoredName = Attri.StoredName) then
        break;
      NewNode := NewNode.GetNext;
    end;}
  end;
  if NewNode <> nil then begin
    NewNode.Selected := True;
    //FindCurHighlightElement;
  end;

end;

function TfrmConfig.GetHighlighter: TSynCustomHighlighter;
begin
  result := self.SynEdit1.Highlighter;
end;

procedure TfrmConfig.SetHighlighter(AValue: TSynCustomHighlighter);
begin
  self.SynEdit1.Highlighter := AValue;
end;

end.

