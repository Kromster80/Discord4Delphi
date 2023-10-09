unit Unit_Form;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Unit_Discord;

type
  TForm1 = class(TForm)
    Button1: TButton;
    meLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  fD4D := TDiscord4Delphi.Create(HandleLog);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fD4D);
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  fD4D.InitDiscord;
end;


procedure TForm1.HandleLog(aText: string);
begin
  meLog.Lines.Append(aText);
end;


end.
