#!/QOpenSys/usr/bin/sh
# ------------------------------------------------------------------------- #
# This program reads current folder from IFS and takes below steps.
# 1. It assumes these sources i.e. dspf, rpgle, sqlrpgle, clle.  
# 2. It copies .rpgle, .sqlrpgle, .clle and .dspf to respective _SRC. 
# 3. It compiles the DSPF first, then RPGLE, SQLRPGLE and finally CLLE. 
# After coping this program to IFS folder where your sources are, use below command
#
# to make this file executable. 
# chmod +x CopyToMbr.sh
# Set the CUR_LIB to your library.
# Check if the SRCPF names are correct for you. 
# Set the application name which will be applied to all the memebers as member text.
# ------------------------------------------------------------------------- #

# Set the IFS directory, if not given, set current
IFS_DIR="${1:-$(pwd)}"

# Set the current library where the programs will be compiled. 
CUR_LIB="S104120X"

# Set source physical files
DDS_SRC="MISC"
RPGLE_SRC="MISC"
CL_SRC="MISC"

# Set application name
APPLICATION="MISC DIRECTORY"


# Copy DSPF to source PF
# Copy RPGLE to SRCPF and compile
crt_rpgle(){
  echo -e '\n... executing commands... '
  filename=$(basename "$1")
  member="${filename%.*}"
  exec_cmd "CHGATR OBJ('$1') ATR(*CCSID) VALUE(819)"
  exec_cmd "CPYFRMSTMF FROMSTMF('$1') TOMBR('/QSYS.lib/$CUR_LIB.lib/$RPGLE_SRC.file/$member.mbr') MBROPT(*REPLACE)"
  exec_cmd "CHGPFM FILE($CUR_LIB/$RPGLE_SRC) MBR($member) SRCTYPE(RPGLE) TEXT('$APPLICATION')"
}


search_rpgle(){
    for FILE in "$IFS_DIR"/*
        do
            ext="${FILE##*.}"
            if [[ $ext == 'rpgle' ]]; then
                echo -e "\n=========== Copying RPGLE - $FILE ============="
                crt_rpgle $FILE 
            fi
        done
}


echo -e "|=== Starting to move the programs for $APPLICATION ===|"
search_rpgle

echo -e '|=================================================================================|'
echo -e '| Program move completed, please check if you encountered any errors.             |'
echo -e "| $IFS_DIR                                                                        |"
echo -e "| $pwd                                                                        |"
echo -e '|=================================================================================|'