{
Sample for a case when the App, written in English, is translated to Spanish.
For that, we must use dic(), in every constant string and translate the  caption
of all the Forms using, TransCapCtrls() or TransCapCtrlsName().
}
unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MisUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  //translate caption of controls
  TransCapCtrls(self, 'Send','Enviar');
  TransCapCtrls(self, 'Name:','Nombre:');
  //add keys for translate the message
  dicSet('Hello','Hola');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  msgBox(dic('Hello')+ ' ' +Edit1.Text);
end;

end.

