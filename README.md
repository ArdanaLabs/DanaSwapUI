# DanaSwapUI
This project serves as the Front-end Web platform for the Ardana Application.

## Build via Nix

List all the outputs of the flake.nix with `nix flake show`

1. Build the project of your choice:

   ```
   nix build .#frontend-landing
   ```

2. See the contents of the `result` file that is produced in the current
   directory.

   ```
   user: matthew swordfish in DanaSwapUI on  flake [?]
   ❯ tree result/lib/node_modules/ardana-application/build/ -L 2
   result/lib/node_modules/ardana-application/build/
   ├── asset-manifest.json
   ├── favicon.ico
   ├── index.html
   ├── logo192.png
   ├── manifest.json
   ├── robots.txt
   └── static
       ├── css
       ├── js
       └── media
   ```

## Build via Npm
To obtain an executable file, run:

```npm run build```

It can also be approached with yarn:

```yarn build```

## Developer setup instructions

Requirements:
- Node.js
- Yarn

Download & Install packages:
- ```npm install``` or ```yarn```


## Developer workflow instructions

To Start Server:
- ```npm start``` or ```yarn start```

To Run Test Suite:
- ```npm run test``` or ```yarn test```

Linting:
- ```npm run lint``` or ```yarn lint```

## Directory structure explanation

### frontend-dashboard | frontend-landing
- ```src/assets``` - the resources contains images, icons, fonts, etc
- ```src/components``` - the source code for commonly used components
- ```src/config``` - the contraints
- ```src/hooks``` - the source code for utility functions
- ```src/layouts``` - the source code for theme template
- ```src/locales``` - the assets for translation
- ```src/pages``` - the source code for pages
- ```src/state``` - the source code for redux setup

## Core branches
- ```main```
- ```staging```
- ```dev```
