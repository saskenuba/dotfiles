#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi


setxkbmap -layout us -variant intl
echo UPDATESTARTUPTTY | gpg-connect-agent
feh --bg-scale ~/Documentos/Images/Wallpaper/restaurant_street_by_arsenixc-dc8by2t.jpg ~/Documentos/Images/Wallpaper/watchmaker_house_by_arsenixc-d9dv2ge.jpg
wmname LG3D &
/home/martin/.start.sh &
parcellite &
slstatus &
exec dwm
