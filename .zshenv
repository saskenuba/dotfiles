alias paste-clipboard='xclip -out -selection clipboard'
alias copy-clipboard='xclip -sel clipboard'

VISUAL=emacs
EDITOR=emacs
RANGER_LOAD_DEFAULT_RC=false

XDG_CONFIG_HOME=$HOME/.config
XDG_CACHE_HOME=$HOME/.cache

# Better performance for emacs LSP-mode https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
LSP_USE_PLISTS=true

transliterate () {

	filename=$1
	extension="${filename##*.}"
	filename="${filename%.*}"

	iconv --from-code utf8 --to-code ASCII//TRANSLIT $1 > $filename-fix.$extension
}

pad-with-color() {
	name=$1
        convert $name.png -background '#8A4A52' -gravity center -extent 1920x1080 $name-padded.png
}

paste-carbon () {

	RANDOM=$RANDOM
	extension=$1
	output_name=$2-$RANDOM

	name=/dev/shm/$RANDOM
        filename=/dev/shm/$RANDOM.$extension

	# paste snippet to file
        paste-clipboard > $filename

	# copy carbon snippet to clipboard
	carbon-now $filename -h -c

	# save carbon snippet to file
	paste-clipboard > $output_name.png

	# padding
	pad-with-color $output_name

	paddedName=$output_name-padded.png
	echo 'Concluído. Arquivo com padding salvo em `realpath .`'$paddedName.
}

ocr-selection() {

	convert x: -modulate 100 -density 300 -resize 400% png:- | tesseract stdin stdout
}
