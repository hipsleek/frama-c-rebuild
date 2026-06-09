##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

ACTION="$*"

if [ "$ACTION" == "" ]
then
    ACTION="status -s -b"
fi

if [ "$1" == "-h" -o "$1" == "--help" ]
then
    echo ""
    echo "Multi-repository git broadcast"
    echo ""
    echo "  git.sh -h|--help"
    echo "  git.sh clone <repo> <dir>"
    echo "  git.sh remove <dir>"
    echo "  git.sh exec <command...>"
    echo "  git.sh cat <file> <command...>"
    echo "  git.sh <command...>"
    echo ""
    echo "Command 'git.sh clone <repo> <dir>' performs"
    echo "an inner clone of <repo> inside <dir> sub-directory"
    echo "and add <dir> to the excluded directories of the root."
    echo ""
    echo "Command 'git.sh remove <dir>' removes the git"
    echo "clone at <dir>, if any."
    echo ""
    echo "Command 'git.sh exec <command>' executes the given command"
    echo "in each git repository."
    echo ""
    echo "Command 'git.sh cat <file> <command>' executes the given command"
    echo "in each git repository and concat their output to the given file."
    echo ""
    echo "Otherwise, 'git.sh <command>' broadcast the command"
    echo "to all '.git' repository accessible from the root."
    echo "The default command is 'status -s -b'."
    echo ""
    exit 0
fi

if [ "$1" == "remove" ]
then
    if [ "$2" == "" ]
    then
        echo "git.sh remove <directory>"
        exit 2
    fi
    if [ ! -d "$2/.git" ]
    then
        echo "Missing git repository $2/.git"
        exit 2
    fi
    echo "--------------------------------------------------"
    echo "-- Removing ./$2"
    echo "--------------------------------------------------"
    rm -fr $2
    grep -v "/$2" .git/info/exclude > .tmp
    mv .tmp .git/info/exclude
    cat .git/info/exclude
    ACTION="status -s -b"
fi

if [ "$1" == "clone" ]
then
    if [ "$2" == "" -o "$3" == "" ]
    then
        echo "git.sh clone <repository> <directory>"
        exit 2
    fi
    echo "--------------------------------------------------"
    echo "-- Repository ./$3"
    echo "--------------------------------------------------"
    echo "Cloning $2"
    mkdir -p $3
    (cd $3 && git clone $2 .)
    echo "--------------------------------------------------"
    echo "--- .git/info/exclude"
    echo "--------------------------------------------------"
    echo "/$3" >> .git/info/exclude
    cat .git/info/exclude
    ACTION="status -s -b"
fi

if [ "$1" == "exec" ]
then
    shift
    ROOT=`pwd`
    COMMAND="$*"
fi

if [ "$1" == "cat" ]
then
    shift
    ROOT=`pwd`
    case $1 in
        /*)
            TARGET="$1"
        ;;
        *)
            TARGET="$ROOT/$1"
        ;;
    esac
    shift
    COMMAND="$*"
    rm -f $TARGET
    touch $TARGET
fi

for pgit in `find . -type d -name .git`
do

    plugin=`dirname $pgit` ;

    echo "--------------------------------------------------"
    echo "-- Repository $plugin"
    echo "--------------------------------------------------"

    if [ "$COMMAND" != "" ]
    then
        cd $plugin
        if [ "$TARGET" != "" ]
        then
            echo "$ $COMMAND >> $TARGET"
            $COMMAND >> $TARGET
        else
            echo "$ $COMMAND"
            $COMMAND
        fi
        cd $ROOT
    else
        git -C $plugin $ACTION
    fi

done
echo "--------------------------------------------------"
