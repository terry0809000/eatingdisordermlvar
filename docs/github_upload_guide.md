# GitHub upload guide

Create an empty GitHub repository named:

```text
eatingdisordermlvar
```

Then run the following commands from Git Bash:

```bash
cd ~/Downloads
unzip eatingdisordermlvar.zip
cd eatingdisordermlvar

git init
git add .
git commit -m "Initial eating disorder mlVAR network analysis repository"
git branch -M main
git remote add origin https://github.com/terry0809000/eatingdisordermlvar.git
git push -u origin main
```

When GitHub asks for a password, paste a GitHub Personal Access Token rather than your account password.

Do not commit raw EMA data or participant-level outputs to a public repository.
