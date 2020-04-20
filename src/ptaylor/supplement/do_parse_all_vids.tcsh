#!/bin/tcsh

# to go through all directories that have videos on afni, and parse text files

set here  = $PWD
set otext = ${here}/o.all_youtube_details.txt

# get dir holding all vids, and name of each subdir that might have vids.
# note that we parse some names out
cd ..
set top_dir = `pwd`
set all_movie_dir = `find . -maxdepth 1 -type d  \
                        | cut -b3-               \
                        | grep -v script         \
                        | grep -v OLD            \
                        | sort`
cd -

printf "" > ${otext}

set count = 0

foreach mdir ( ${all_movie_dir} )

    cd ${top_dir}/${mdir}

    set all_movies = `find ./ -maxdepth 1 -type f \
                        \( -iname "*.mp4" -o -iname "*.flv" \) \
                        | sort`

    if ( "${#all_movies}" != "0" ) then

        foreach ff ( ${all_movies} )

            printf "++ Found movie [ %03d ]:  ${mdir}/${ff}\n" ${count}

            echo "-------------------------------------------------------" >> ${otext}
            echo "${count}"      >> ${otext}
            echo "${mdir}/${ff}" >> ${otext}

            python ${here}/parse_video_text.py \
                    -movie ${ff}               \
                    >> ${otext} 

            @ count+= 1
        end
    endif

end

echo ""
echo "++ DONE"
echo "   Number of found movies:  ${count}"
echo "   See output script:"
echo "       ${otext}"                      


