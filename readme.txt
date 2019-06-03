mkdir parcel-demo
cd parcel-demo/
yarn add parcel-bundler
yarn init -y
elm init

cat << 'EOF' > src/index.html
<!-- index.html -->

<html>
  <body>
    <main></main>
    <script src="./index.js"></script>
  </body>
</html>
EOF

cat << 'EOF' > src/index.html
// index.js

import { Elm } from './Main.elm'

Elm.Main.init({
  node: document.querySelector('main')
})
EOF

#create src/Main.elm

# start parcel with hot reload
parcel serve src/index.html 

#optionally add script to package.json
--snip--
  "scripts": {
    "start": "parcel serve src/index.html"
  }
--snip--
#yarn start
