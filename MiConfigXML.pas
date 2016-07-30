{
MiConfigXml 0.1b
=============
Por Tito Hinostroza 29/07/2016

Descripción
===========
Unidad con rutinas de lectura/escritura de propiedades en archivos XML. Permite crear
fácilmente, una ventana de configuración, con las opciones: ACEPTAR y CANCELAR.
Es similar a MiConfigXML, pero trabaja con archivos XML.

Para alamacenar las propiedades, se debe crear un objeto TMiConfigINI. Sin embargo,
la unidad crea por defecto, una isntancia de TMiConfigINI, llamada "xmlFile", que toma
como nombre <nombre del proyecto.ini>
Tiene como dependencia a la librería MisUtils.

Por Tito Hinostroza 29/07/2016
}
unit MiConfigXML;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, Forms, Laz2_XMLCfg, MisUtils,
  MiConfigBasic;

type
  { TMiConfigXML }
  {Clase base que es usada para manEjar lso campos de configuración.}
  TMiConfigXML = class(TMiConfigBasic)
  private
    XMLfile    : string;   //archivo XML
    function DefaultFileName: string;
    procedure FileProperty(xmlCfg: TXMLConfig; const r: TParElem; FileToProp: boolean);
  public
    secINI: string;   //sección donde se guardarán los datos en un archivo INI
    property FileName: string read XMLfile write XMLfile;
    procedure VerifyFile;
    function FileToProperties: boolean; virtual;
    function PropertiesToFile: boolean; virtual;
  public  //Constructor y Destructor
    constructor Create(XMLfile0: string);
    destructor Destroy; override;
  end;

var
  xmlFile : TMiConfigXML;   //Default XML Config file

implementation



{ TMiConfigXML }
function TMiConfigXML.DefaultFileName: string;
{Devuelve el nombre pro defecto del archvio de configuración}
begin
  Result := ChangeFileExt(Application.ExeName,'.xml');
end;
procedure TMiConfigXML.VerifyFile;
//Verifica si el archivo XML "FileName" existe. Si no, muestra un mensaje y lo crea.
var
  F: textfile;
begin
  if not FileExists(FileName) then begin
    MsgErr('No XML file found: %s', [FileName]);
    //crea uno vacío para leer las opciones por defecto
    AssignFile(F, FileName);
    Rewrite(F);
    writeln(F, '<?xml version="1.0" encoding="utf-8"?>');
    writeln(F, '<CONFIG>');
    writeln(F, '</CONFIG>');
    CloseFile(F);
  end;
end;
procedure TMiConfigXML.FileProperty(xmlCfg: TXMLConfig; const r: TParElem; FileToProp: boolean);
{Permite leer o escribir una propiedad en el archivo XML}
var
  n: Integer;
  s: String;
begin
  case r.tipPar of
  tp_Int_TEdit:
    if FileToProp then begin  //lee entero
      //Integer(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
      Integer(r.Pvar^) := xmlCfg.GetValue(r.etiqVar + '/Val', r.defEnt)
    end else begin  //escribe entero
      n := Integer(r.Pvar^);
//      arcINI.WriteInteger(secINI, r.etiqVar, n);
      xmlCfg.SetValue(r.etiqVar + '/Val', n) ;
    end;
{    tp_Int_TSpinEdit: begin //escribe entero
       n := Integer(r.Pvar^);
       arcINI.WriteInteger(secINI, r.etiqVar, n);
     end;
  tp_Dbl_TEdit: begin  //escribe double
       d := Double(r.Pvar^);
       arcINI.WriteFloat(secINI, r.etiqVar, d);
  end;
  tp_Dbl_TFloatSpinEdit: begin
       d := Double(r.Pvar^);
       arcINI.WriteFloat(secINI, r.etiqVar, d);
  end;
}
  tp_Str_TEdit:
    if FileToProp then begin  //lee
//      string(r.Pvar^) := DecodeStr(arcINI.ReadString(secINI, r.etiqVar, '.'+r.defStr+'.'));
      string(r.Pvar^) := xmlCfg.GetValue(r.etiqVar + '/Val', r.defStr);
    end else begin //escribe cadena
      s := String(r.Pvar^);
//         arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
      xmlCfg.SetValue(r.etiqVar + '/Val', s) ;
    end;
{    tp_Str_TEditButton: begin //escribe cadena
       s := String(r.Pvar^);
       arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
     end;
  tp_Str_TCmbBox: begin //escribe cadena
       s := String(r.Pvar^);
       arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
     end;
  tp_StrList_TListBox: begin  //escribe TStringList
        strlst := TStringList(r.Pvar^);
        arcINI.EraseSection(secINI+'_'+r.etiqVar);
        for j:= 0 to strlst.Count-1 do begin
          arcINI.WriteString(secINI+'_'+r.etiqVar,
                             CodeStr(strlst[j]),'');
        end;
     end;
  tp_Bol_TCheckBox: begin  //escribe booleano
       b := boolean(r.Pvar^);
       arcINI.WriteBool(secINI, r.etiqVar, b);
     end;
  tp_TCol_TColBut: begin  //escribe TColor
       c := Tcolor(r.Pvar^);
       arcINI.WriteInteger(secINI, r.etiqVar, c);
     end;
  tp_Enum_TRadBut: begin  //escribe enumerado
     if r.lVar = 4 then begin
       n := Int32(r.Pvar^);   //lo guarda como entero
       arcINI.WriteInteger(secINI, r.etiqVar, n);
     end else begin  //tamaño no implementado
       msjErr := dic('Enumerated type no handled.');
       exit;
     end;
  end;
  tp_Enum_TRadGroup: begin  //escribe enumerado
     if r.lVar = 4 then begin
       n := Int32(r.Pvar^);   //lo guarda como entero
       arcINI.WriteInteger(secINI, r.etiqVar, n);
     end else begin  //tamaño no implementado
       msjErr := dic('Enumerated type no handled.');
       exit;
     end;
  end;
  tp_Bol_TRadBut: begin  //escribe booleano
       b := boolean(r.Pvar^);
       arcINI.WriteBool(secINI, r.etiqVar, b);
     end;
  tp_Int: begin //escribe entero
       n := Integer(r.Pvar^);
       arcINI.WriteInteger(secINI, r.etiqVar, n);
     end;
  tp_Bol: begin  //escribe booleano
       b := boolean(r.Pvar^);
       arcINI.WriteBool(secINI, r.etiqVar, b);
     end;
  tp_Str: begin //escribe cadena
       s := String(r.Pvar^);
       arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
     end;
  tp_StrList: begin  //escribe TStringList
        strlst := TStringList(r.Pvar^);
        arcINI.EraseSection(secINI+'_'+r.etiqVar);
        for j:= 0 to strlst.Count-1 do begin
          arcINI.WriteString(secINI+'_'+r.etiqVar,
                             CodeStr(strlst[j]),'');
        end;
     end;}
  else  //no se ha implementado bien
    msjErr := dic('Design error.');
    exit;
  end;
end;
function TMiConfigXML.FileToProperties: boolean;
{Lee de disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr".}
var
  r: TParElem;
  xmlCfg: TXMLConfig;
begin
  if not FileExists(fileName) then begin
    MsjErr := dic('XML file does not exist.');  //errro
    exit(false);  //para que no intente leer
  end;
  //Asume error por defecto
  Result := false;
  MsjErr := dic('Error reading INI file: %s', [fileName]);
  try
    xmlCfg := TXMLConfig.Create(nil);
    xmlCfg.Filename := XMLfile;  //lee archivo XML
    for r in listParElem do begin
      FileProperty(xmlCfg, r, true);
    end;
    Result := true;  //sin error
    MsjErr := '';    //sin error
  finally
    xmlCfg.Destroy;
  end;
end;
function TMiConfigXML.PropertiesToFile: boolean;
{Guarda en disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr".}
var
  r: TParElem;
  xmlCfg: TXMLConfig;
begin
  Result := false;
  MsjErr := dic('Error writing INI file: %s', [fileName]);
  try
    If FileExists(fileName)  Then  begin  //ve si existe
       If FileIsReadOnly(fileName) Then begin
          MsjErr := dic('INI file is only read.');
          exit(false);
       End;
    End;
    xmlCfg := TXMLConfig.Create(nil);
    xmlCfg.Filename := XMLfile;  //lee archivo XML
    xmlCfg.Clear;
    for r in listParElem do begin
      FileProperty(xmlCfg, r, false);
    end;
    xmlCfg.Flush;
    Result := true;  //sin error
    MsjErr := '';    //sin error
  finally
    xmlCfg.Destroy;
  end;
end;
//Constructor y Destructor
constructor TMiConfigXML.Create(XMLfile0: string);
begin
  inherited Create;
  XMLfile := XMLfile0;
end;
destructor TMiConfigXML.Destroy;
begin
  inherited Destroy;
end;

initialization
  xmlFile := TMiConfigXML.Create(xmlFile.DefaultFileName);

finalization
  xmlFile.Destroy;
end.

