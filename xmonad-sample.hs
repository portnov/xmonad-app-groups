
module Main where

import Data.Default
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.WindowProperties
import XMonad.Util.WindowPropertiesRE
import XMonad.Config.Desktop
import XMonad.AppGroups

regex = C . RE . ClassName

myApps = 
  [ [C "Epiphany-browser", C "Kontact",
     C "Iceweasel", C "Firefox", C "Chromium", C "Vivaldi-stable",
     C "Opera", C "Arora" ]     `orSpawn` "firefox"          ~> "inet"     `on` "M1-x w" `named` "web",
    [C "Icedove", C "Kontact" ] `orSpawn` "icedove"          ~> "news"     `on` "M1-x y" `named` "mail",
    group [C "Inkscape", C "Eog", C "Gwenview", C "Dia",
                   C "MyPaint"]                              ~> "graphics" `on` "M1-x d",
    group         [regex "Gimp"]                             ~> "gimp"     `on` "M1-x g",
    group [C "F-spot", regex "Digikam", regex "Darktable"]   ~> "photo"    `on` "M1-x p",
    [C "Gnome-terminal", C "konsole"]   `orSpawn` "konsole" ~>> "term"     `on` "M1-x t",
    [C "Gedit", C "Leafpad",
     C "Text-terminal", C "Gvim",
     C "kate", C "kwrite", C "Emacs"] `orSpawn` "gvim"      ~>> "text"     `on` "M1-x e",
    group [regex "libreoffice", C "TeXmacs"]                ~> "office"   `on` "M1-x o",
    group [C "Evince", C "Okular"]                          ~> "docs"     `on` "M1-x k",
    [C "Nautilus", C "dolphin", C "Konqueror", C "Pcmanfm",
                   C "Krusader"] `orSpawn` "dolphin"        ~>> "files"    `on` "M1-x f",
    group [C "Amarok", C "Rhythmbox", C "Ario",
           C "Sonata", regex "Audacious" ]                   ~> "music"    `on` "M1-x a",
    group         [C "MPlayer", C "Totem"]                  ~>> "video"    `on` "M1-x v",
    [C "Pidgin", C "Kopete"]          `orSpawn` "pidgin"                   `on` "M1-x i" ]


appsConfig :: AppsConfig
appsConfig = AppsConfig myApps def def def $ fromGroups $
  [["inet", "news", "mail", "text", "office", "video", "photo", "graphics", "gimp"],
   ["plots", "dashboard2"]]

main :: IO ()
main = do
  xmonad $ desktopConfig `additionalKeysP` apps2keys appsConfig

