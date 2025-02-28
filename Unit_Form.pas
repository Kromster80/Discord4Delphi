unit Unit_Form;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Unit_Discord, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    meLog: TMemo;
    btnChangeActivity: TButton;
    Timer1: TTimer;
    bynClearActivity: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnChangeActivityClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure bynClearActivityClick(Sender: TObject);
  private
    fD4D: TDiscord4Delphi;
    procedure HandleLog(aText: string);
  end;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  fD4D := TDiscord4Delphi.Create({$I client.inc}, HandleLog);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fD4D);
end;


procedure TForm1.btnChangeActivityClick(Sender: TObject);
begin
  fD4D.ActivityChange('Details of the activity', 'State', Now - 1/48, Now + 1/48);
end;


procedure TForm1.bynClearActivityClick(Sender: TObject);
begin
  fD4D.ActivityClear;
end;


procedure TForm1.HandleLog(aText: string);
begin
  meLog.Lines.Append(aText);
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  fD4D.Update;
end;


end.
