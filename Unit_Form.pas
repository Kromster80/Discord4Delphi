unit Unit_Form;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Unit_Discord, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    meLog: TMemo;
    Button2: TButton;
    Timer1: TTimer;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    fD4D: TDiscord4Delphi;
    procedure HandleLog(aText: string);
  end;

var
  Form1: TForm1;

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


procedure TForm1.Button2Click(Sender: TObject);
begin
  fD4D.ActivityChange('SomeDetails', 'SomeState', Now - 1/48, Now + 1/48);
end;


procedure TForm1.Button3Click(Sender: TObject);
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
