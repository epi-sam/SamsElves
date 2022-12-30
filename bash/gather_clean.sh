# This is a function to clean a repository per GATHER requirements for public distribution
# Written primarily for R files, it will remove quoted filepaths among many other things.
# Additional inspection of python and other file types is recommended.
# Several git grep suggestions are made at the end to ease the visual inspection after cleaning.
# 
# Please note there is technically profanity in this script, which searches for inappropriate language during cleaning.
#
# Please suggest edits and refinements to the author!


gather_clean() {

  echo ""
  echo "Running multiple cleaning functions to prepare for GATHER compliance. 
  If you want to run cleaners individually, 
  press 'ctrl + C' to cancel, 
  copy and paste cleaners individually from the function body."
  echo ""
  echo "Please commit any important changes to your repo BEFORE running this cleaner."
  echo ""
  echo "If you do not like the results, 
  use 'git reset --hard' before committing changes.  
  This will undo all changes from the cleaner 
  (and any others you made since your last commit). 
  Then you can alter function parameters and try again."
  echo ""

  # ask user preferences
  # if preferences are not supplied, set defaults

  read -p "Would you like to search your repo for file types/extensions that may need cleaning? (type 'y' if yes) " yn_filetype

  if [ "$yn_filetype" == "y" ]; then
   echo "Searching repo for all file extensions"
   find . -type f | perl -ne 'print $1 if m/\.([^.\/]+)$/' | sort -u
  fi

  read -p "File extension type to clean (case sensitive) (default regex = [rR]): " re_ext

  if [ -z "$re_ext" ]; then
    echo "No extension supplied, defaulting to [rR]"
    local re_ext="${1:-[rR]}"
  fi

  read -p "Comment regex to clean (removes entire comment - case sensitive) (e.g. FIXME|TODO|NOTE|HOTIX|hotfix): " re_comment

  if [ -z "$re_comment" ]; then
    echo "No comments supplied, defaulting to FIXME|TODO|NOTE|SB|HOTIX|hotfix"
    local re_comment="${1:-FIXME|TODO|NOTE|SB|HOTIX|hotfix}"
  fi

  read -p "Name regex to clean (removes word - case insensitive) (e.g. steve|lim|bobby|reiner|john|giles|etc...): " re_names

  if [ -z "$re_names" ]; then
    local re_names="${1:-steve|lim|chris|troeger|bobby|reiner|john|giles|james|collins|ryan|barber|kaleb|coberly|sam|byrne|austin|carter|emma|castro|erin|tom|pham|parkes|kendrick|haley|lecsinsky|Lescinsky|jeremy|dalos|alice|lazzar|atwood|bachmeier|lauren|woyczynski|Brittney|Sheena|Emily|Linebarger|Brittney|leonardo|Kate|Causey|Xiaohcen|Xiaochen|Dai|Lucas|Earl|Anoushka|Millear|Marissa|Reitsma|Edem|Dossou|Fleming|Katrin|Burkart|Bianca|Zlavog|Peng|Zheng|Sasha|Aravkin}"
    echo "No names supplied, defaulting to (case insensitive): $re_names"
  fi

  read -p "Other words to redact (removes word - case insensitive) (defaults to common profanity): " re_profanity

  if [ -z "$re_profanity" ]; then
    echo "No keywords supplied, defaulting to common profanity (case insensitive)"
    local re_profanity="${1:-hotfix|damn|shit|fuck|hell|heck|ass|bitch|dick}"
  fi

  read -p "Number of times to run SYSTEM_COMMAND replacement (one cycle per newline suggested) (default = 1): " int_repeat

  if [ -z "$int_repeat" ]; then
    echo "No repeat supplied, defaulting to 1"
    local int_repeat="${1:-1}"
  fi

  echo ""

  # do the cleaning
  echo "Cleaning files with extension =" "*.$re_ext"

  # strip cluster references (removes word - case insensitive)
  echo "Replacing cluster references with CLUSTER_NAME: " "#.*(?: slurm | uge | sge ).*"
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@#.*(?: slurm | uge | sge ).*@CLUSTER_NAME@gi"

  # strip names (removes word - case insensitive)
  echo "Replacing known contributer names with INDIVIDUAL_NAME"
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@(?<=[\s\)\p{P}])($re_names)(?=[\s\)\p{P}])@INDIVIDUAL_NAME@gi"

  # strip emails (removes word - case insensitive)
  echo "Replacing email addresses with EMAIL_ADDRESS"
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@([a-zA-Z0-9\.\_\-]+\@[a-zA-Z0-9\.\_\-]+\.[a-zA-Z0-9\_\-]+)@EMAIL_ADDRESS@gi"
  
  # strip comments (removes entire comment - case sensitive)
  echo "Cleaning out comments (lines deleted): " $re_comment
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@(#.*(?:$re_comment).*)@@g"

  # strip other key words or profanity (removes word - case insensitive)
  echo "Replacing keywords / profanity with REDACTED"
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@(?<=[\s\)\p{P}])($re_profanity)(?=[\s\)\p{P}])@REDACTED@gi"

  # clean paths before files
  echo "Replacing quoted '/path/file.ext' with FILEPATH, preserving file name and extension"
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@(?<=[\'\"])(\s*-*\w*\s*\w*\s+)?[\w-.{} ]*[\w-]?\/+[\w-\/.%{} ]*(?=\/[\w.-]*\.[A-Za-z-]*[\'\"])@\1FILEPATH@g"

  # clean lone paths
  echo "Replacing lone quoted 'file/paths' with FILEPATH"
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@(?<=[\'\"])(\s*-*\w*\s*\w*\s+)?[\w-. ]*[\w-]?\/+[^()\'\"\n]*\/[\w\[\]{}:+-]*[.0-9]*(?=[\'\"])@\1FILEPATH@g"

  echo "Replacing contents of any system(2)() commands with SYSTEM_COMMAND, preserving nested functions and parentheses - please check outputs.  Requires looping once per newline the system command is spread over."

  # run multiple times to clean multiple lines
  echo "Repeating SYSTEM_COMAND replacement $int_repeat times"
  for i in $(seq $int_repeat); do
    echo "$i"
    find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@(system[2]?[\(])((?:[\w\.\:]*[\(]+)*)[^\)]*(?=[\)]*)@\1\2SYSTEM_COMMAND@g"
  done

  echo "Clean particular comments with likely file paths - this cleaner is pretty rough, inspect outputs"
  find . -type f -iname "*.$re_ext" | xargs perl -pi -e "s@([# ]*).*ihme\.slack\.com.*|.*help \= .*[\'\"].*\/.*[\'\"]@\1INTERNAL_COMMENT@g"

  echo ""
  echo "This cleaner is selective, focused on R code formatting, and could miss some burried strings and comments, especially in other file formats.

  Additional limited searching by hand is a good idea.
  
  Recommend running the following:
  
  to see all replacements: git grep -nE 'CLUSTER_NAME|INDIVIDUAL_NAME|EMAIL_ADDRESS|REDACTED|FILEPATH|SYSTEM_COMAND|INTERNAL_COMMENT' 

  to also see all changes:  git diff

  to see summary of changes:  git diff --stat <comparison_branch>

  to see tally of all insertions and deletions:  git diff --shortstat <comparison_branch>
  
  to check for lingering names associated with common terms: git grep -nEi 'author|written|maintained'
  
  to find anything quoted with slashes: git grep -n '[\'\"].*/.*[\'\"]' ./path/to/your/subfolder

  to find slashed strings in comments that may not be quoted:  git grep -n '#.*\/.*\/.*' ./path/to/your/subfolder"

  echo ""
  echo "If results are not as desired, 
  run 'git reset --hard' before committing results as the 'undo button', 
  change function inputs, and rerun."
  echo ""
  echo "Remember it's helpful to delete HTML, PDF and other non-code files before using git grep."
  echo ""
  echo "Remember you need to delete the '.git' and '.gitignore' folders 
  from the repo after all cleaning is done, before handing the code off, 
  since it contains the entire history of the code base."

}