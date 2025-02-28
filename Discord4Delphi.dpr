program Discord4Delphi;
uses
  Vcl.Forms,
  Unit_Form in 'Unit_Form.pas' {Form1},
  Unit_DiscordDLL in 'Unit_Discord.pas',
  Unit_DiscordTypes in 'Unit_DiscordTypes.pas';

{$R *.res}

var
  Form1: TForm1;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
