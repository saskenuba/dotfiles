alias poetry='~/.poetry/bin/poetry'
alias paste-clipboard='xclip -out -selection clipboard'

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


