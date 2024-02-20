echo "Rendering the dashboard..."
if [[ "$1" = ""  || "$2" = "" ]] ; then
    echo "The git user.name and/or user.email are missing"
    exit 0
else
    echo "Git user.name is $1"
    echo "Git user.email is $2"
fi


quarto render

# Fix github issue
git config --global --add safe.directory /__w/r_docker_vscode/r_docker_vscode


if [[ "$(git status --porcelain)" != "" ]]; then
    git config --global user.name $1
    git config --global user.email $2
    git add *
    git commit -m "Auto update dashboard"
    git push
fi