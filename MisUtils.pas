{MisUtils 0.2
 =============
 Por Tito Hinostroza 17/10/2014
 * Se agregan nuevas formas de MsgExc() y MsgExc() para que tengan las mismas
 funcionalidades que MsgBox().
 * Se corrige uso de parámetro "Caption" en MsgExc()

 Descripción
 ============
 Librería de funciones útiles para mostrar mensajes en pantalla, para guardar datos en
 archivos, para crear aplicaciones en varios idiomas y algunas utilidades adicionales.
 }
unit MisUtils;

{$mode objfpc}{$H+}

interface

uses  Classes, SysUtils, Forms, Graphics, Dialogs, process, Controls,
      lclType, FileUtil, types, dateutils, Menus;

var
  msjError  : string;       //mensaje de error de la aplicación
  dictionary: TstringList;  //diccionario para el manejo de mensajes
  TranslateMsgs: boolean;   //activa la traducción del mensaje

procedure MsgExc(txt: string; Caption: string = '');
procedure MsgExc(Fmt: String; const Args: array of const);
procedure MsgErr(txt: string; Caption: string = '');
procedure MsgErr(Fmt: String; const Args: array of const);
//function MsgBox(txt: PChar; Caption: string = ''; flags: longint = 0): integer;
function MsgBox(txt: String; Caption: string = ''; flags: longint = 0): integer;
procedure MsgBox(Fmt : String; const Args : Array of const);
function MsgYesNo(txt: string): byte;
function MsgYesNo(Fmt: string; const Args: array of const): byte;
function MsgYesNoCancel(txt: string): byte;
function MsgYesNoCancel(Fmt: string; const Args: array of const): byte;

function Explode(delimiter:string; str:string):TStringDynArray;
function Exec(com: string): boolean;
procedure StringToFile(const s: string; const FileName: string);
function StringFromFile(const FileName: string): string;
//Utilidades para menús
function AddItemToMenu(menu: TMenuItem; txt: string; evento: TNotifyEvent): TMenuItem;
procedure CheckOnlyOneItem(item: TMenuItem);
procedure CheckOnlyOneItem(Menu: TMenuItem; Caption: string);
//Genera un nombre distinto de archivo
function GetNewFileName(nomBase: String; maxNumFile: integer = 10): String;
//Genera un nombre distinto de carpeta
function GetNewFolderName(nomBase: String; maxNumFile: integer = 10): String;
//Conversion de tipos a cadena
Function f2N(s : String): Double;
Function B2f(b : Boolean) : String;
Function f2B(s : String) : Boolean;
Function D2f(d : TDateTime): String;
Function f2D(s : String) : TDateTime;

//Funciones del diccionario
procedure dicClear;  //limpia el diccionario
procedure dicSet(key, value: string);  //fija una entrada del diccionario
procedure dicDel(key: string);  //limpia una entrada del diccionario
procedure TransCapCtrls(TheForm: TForm; Caption, value: string);  //traduce un mensaje de un control
function dic(key: string): string;     //lee un mensaje traducido
function dic(Fmt : String; const Args : Array of const): string; //lee un mensaje traducido

implementation

const szChar = SizeOf(Char);

procedure MsgExc(txt: string; Caption: string = '');
//Mensaje de exclamación
begin
  if TranslateMsgs then txt := dic(txt);
  Application.MessageBox(PChar(txt), PChar(Caption), MB_ICONEXCLAMATION);
end;
procedure MsgExc(Fmt: String; const Args: array of const);
var
  txt: String;
begin
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  Application.MessageBox(Pchar(txt), '', MB_ICONEXCLAMATION);
end;
procedure MsgErr(txt: string; Caption: string = '');
//Mensaje de error
begin
  if TranslateMsgs then txt := dic(txt);
  Application.MessageBox(PChar(txt), PChar(Caption), MB_ICONERROR);
end;
procedure MsgErr(Fmt: String; const Args: array of const);
var
  txt: String;
begin
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  Application.MessageBox(Pchar(txt), '', MB_ICONERROR);
end;
{function MsgBox(txt: PChar; Caption: string = ''; flags: longint = 0): integer;
begin
  if TranslateMsgs then txt := dic(txt);
  Result := Application.MessageBox(txt, PChar(Caption), flags);
end;}
function MsgBox(txt: String; Caption: string = ''; flags: longint = 0): integer;
begin
  if TranslateMsgs then txt := dic(txt);
  Result := Application.MessageBox(Pchar(txt), PChar(Caption), flags);
end;
procedure MsgBox(Fmt: String; const Args: array of const);
var
  txt: String;
begin
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  Application.MessageBox(Pchar(txt), '', 0);
end;

function MsgYesNo(txt: string): byte;
//Muestra un mensaje en pantalla con los botones Yes - No
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
var
  r: Integer;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then txt := dic(txt);
  r := Application.MessageBox(PChar(txt),'',MB_YESNO + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
end;

function MsgYesNo(Fmt: string; const Args: array of const): byte;
//Muestra un mensaje en pantalla con los botones Yes - No
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
var
  r: Integer;
  txt: String;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  r := Application.MessageBox(PChar(txt),'',MB_YESNO + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
end;

function MsgYesNoCancel(txt: string): byte;
//Muestra un mensaje en pantalla con los botones Yes - No - Cancel
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
//Devuelve 3, si para la opción Cancel
var
  r: Integer;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then txt := dic(txt);
  r := Application.MessageBox(PChar(txt),'',MB_YESNOCANCEL + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
  if r = IDCANCEL  then exit(3);
end;

function MsgYesNoCancel(Fmt: string; const Args: array of const): byte;
//Muestra un mensaje en pantalla con los botones Yes - No - Cancel
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
//Devuelve 3, si para la opción Cancel
var
  r: Integer;
  txt: String;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  r := Application.MessageBox(PChar(txt),'',MB_YESNOCANCEL + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
  if r = IDCANCEL  then exit(3);
end;

function Explode(delimiter:string; str:string):TStringDynArray;
var
  p, n, dsize:integer;
begin
  n := 0;
  dsize := length(delimiter);
  while true do begin
    p := pos(delimiter,str);
    if p > 0 then begin
      inc(n);
      SetLength(Result,n);
      Result[n-1] := copy(str,1,p-1);
      delete(str,1,p+dsize-1);
    end else break;
  end;
  inc(n);
  SetLength(Result,n);
  Result[n-1] := str;
end;

function Exec(com: string): boolean;
//Ejecuta un programa. Devuelve FALSE si hubo error
var
  p    : TProcess;   //el proceso a manejar
begin
  Result := true;
  p := TProcess.Create(nil); //Crea proceso
//  p.CommandLine := SysToUTF8(Application.ExeName +  arc0 + parMsg);
  p.CommandLine := SysToUTF8(com);
  try
    p.Execute;
  except
    Result := false;
    MsgBox('Fallo al iniciar aplicativo: '+ p.CommandLine);;
  end;
  p.Free;
end;

procedure StringToFile(const s: string; const FileName: string);
///Guarda una cadena a un archivo. El archivo debe estar la codificaión del sistema.
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteBuffer(Pointer(s)^, (Length(s) * szChar));
  finally
    FreeAndNil(FileStream);
  end;
end;
function StringFromFile(const FileName: string): string;
//Lee un archivo como una cadena.
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, (FileStream.Size div szChar));
    FileStream.ReadBuffer(Pointer(Result)^, FileStream.Size);
  finally
    FreeAndNil(FileStream);
  end;
end;
//Utilidades para menús
function AddItemToMenu(menu: TMenuItem; txt: string; evento: TNotifyEvent
  ): TMenuItem;
//Agrega un ítema un menú. Devuelve la refrecnia ál nuevo ítem agregado.
var
  item: TMenuItem;
begin
  item := TMenuItem.Create(nil);
  item.Caption:= txt;  //nombre
  item.OnClick:=evento;
  menu.Add(item);
  Result := item;
end;
procedure CheckOnlyOneItem(item: TMenuItem);
//Marca un ítem de un menú y deja los demás desmarcados
var
  MenuPadre: TMenuItem;
  i: Integer;
begin
  MenuPadre := item.Parent;
  if MenuPadre= nil then exit;
  for i:=0 to MenuPadre.Count-1 do  //limpia todos
    MenuPadre.Items[i].Checked := false;
  item.Checked:=true;  //marca el ítem
end;
procedure CheckOnlyOneItem(Menu: TMenuItem; Caption: string);
//Marca un ítem de un menú (usando su etiqueta) y deja los demás desmarcados.
//Ignora la caja y el símbolo "&".
var
  i: Integer;
  capItem: String;
  it: TMenuItem;
begin
  if Menu = nil then exit;  //proteción
  //busca el ítem por su etiqueta
  it := nil;
  Caption := UpCase(Caption);
  for i:=0 to Menu.Count-1 do begin
    capItem := Upcase(Menu.Items[i].Caption);
    capItem := StringReplace(capItem,'&','',[rfReplaceAll]);
    if capItem = Caption then begin
      it := Menu.Items[i];
      break;
    end;
  end;
  if it = nil then exit;   //no encontró
  CheckOnlyOneItem(it);   //marca
end;

function GetNewFileName(nomBase: String; maxNumFile: integer = 10): String;
{Genera un nombre diferente de archivo, tomando el nombre dado como raiz.}
var i : Integer;    //Número de intentos con el nombre de archivo de salida
    cadBase : String;   //Cadena base del nombre base
    extArc: string;    //extensión

  function NombArchivo(i: integer): string;
  begin
    Result := cadBase + '-' + IntToStr(i) + extArc;
  end;

begin
   Result := nomBase;  //nombre por defecto
   extArc := ExtractFileExt(nomBase);
   if ExtractFilePath(nomBase) = '' then exit;  //protección
   //quita ruta y cambia extensión
   cadBase := ChangeFileExt(nomBase,'');
   //busca archivo libre
   for i := 0 to maxNumFile-1 do begin
      If not FileExists(NombArchivo(i)) then begin
        //Se encontró nombre libre
        Exit(NombArchivo(i));  //Sale con nombre
      end;
   end;
   //todos los nombres estaban ocupados. Sale con el mismo nombre
End;
function GetNewFolderName(nomBase: String; maxNumFile: integer = 10): String;
{Genera un nombre diferente de archivo, tomando el nombre dado como raiz.}
var i : Integer;    //Número de intentos con el nombre de archivo de salida
    cadBase : String;   //Cadena base del nombre base

  function NombFolder(i: integer): string;
  begin
    Result := cadBase + '-' + IntToStr(i);
  end;

begin
   Result := nomBase;  //nombre por defecto
//   cadBase := ExtractFilePath(nomBase);
   cadBase := nomBase;
   if cadBase = '' then exit;  //protección
   //busca archivo libre
   for i := 0 to maxNumFile-1 do begin
      If not DirectoryExists(NombFolder(i)) then begin
        //Se encontró nombre libre
        Exit(NombFolder(i));  //Sale con nombre
      end;
   end;
   //todos los nombres estaban ocupados. Sale con el mismo nombre
End;

//############## Funciones de conversión de datos para acceso a disco ############
{Function N2f(n As Single):String;
//Convierte número a cadena para guardar en disco. Independiente de la configuración regional
begin
    N2f = Replace(CStr(n), ",", ".")    //asegura que se usa siempre el punto decimal
End;
}
function f2N(s: String): Double;
//Convierte cadena de disco a número. Independiente de la configuración regional
begin
    Result := StrToFloat(s);     //usa siempre el punto decimal
End;

function B2f(b: Boolean): String;
//Convierte Boleean a cadena para guardar en disco.
begin
    If b Then Result := 'V' Else Result := 'F';
End;

function f2B(s: String): Boolean;
//Convierte cadena de disco a Boleean
begin
    If s = 'V' Then exit(True) else exit(False);
End;

function D2f(d: TDateTime): String;
//Convierte fecha a cadena para guardar en disco.
var
  s: string;
begin
  DateTimeToString(s,'yyyy:mm:dd:hh:nn:ss',d);
  Result :=  s;
End;

function f2D(s: String): TDateTime;
//Convierte cadena de disco a fecha.
var a: TStringDynArray;
begin
//    If (s = '0') Or (s = '') Then Exit Function   //para proteger de las versiones anteriores
  a := explode(':',s);
  Result := EncodeDateTime(StrToInt(a[0]), StrToInt(a[1]), StrToInt(a[2]),
                           StrToInt(a[3]), StrToInt(a[4]), StrToInt(a[5]), 0);
End;

{
Function S2f(s : String) : String;
//Convierte cadena a formato para guardar en disco.
var tmp : String;
begin
    tmp = Replace(s, vbTab, "\t")   'no se permiten tabulaciones
    S2f = Replace(s, vbCrLf, "\n")  'tampoco saltos de línea
End;

Function f2S(s : String) : String;
//Convierte cadena de disco a cadena.
var tmp : String;
begin
    tmp = Replace(s, "\t", vbTab)   'recupera tabulaciones
    f2S = Replace(s, "\n", vbCrLf)  'y saltos de línea
End;
}

procedure dicClear;
//Limpia el diccionario, de modo que no se traducirá ningún mensaje
begin
  dictionary.Clear;
end;
procedure dicSet(key, value: string);
//Fija o agrega una entrada al diccionario
begin
  //los símbolos "=", no se pueden ingresar
  key := StringReplace(key, '=', #31, [rfReplaceAll]);
  dictionary.values[key]:=value;
end;
procedure dicDel(key: string);
//Limpia una entrada del diccionario
begin
  dictionary.values[key]:='';
end;

procedure TransCapCtrls(TheForm: TForm; Caption, value: string);
//Traduce la etiqueta de un control de un formulario
var
  c: TControl;
  i : integer;
begin
   for i := 0 to TheForm.ControlCount-1 do begin
     c := theForm.Controls[i];
     if c.Caption = Caption then c.Caption := value;
   end;
end;

function dic(key: string): string;
//Devuelve un mensaje en el lenguaje definido, dada la clave.
//La clave no puede tener el signo "="
begin
  key := StringReplace(key, '=', #31, [rfReplaceAll]);  //codifica la clave
  Result := dictionary.Values[key];
  //si no enecuentra, devuelve la misma clave
  if Result = '' then Result := key;
end;
function dic(Fmt: String; const Args: array of const): string;
var
  txt: String;
begin
  txt := dic(Fmt);  //busca
  Result := Format(txt, Args);  //completa
end;

Initialization
  //crea diccionario
  dictionary := TStringList.Create;
  TranslateMsgs := false;
Finalization

  dictionary.Destroy;
end.

