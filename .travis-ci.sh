OPAM_DEPENDENCIES="ocamlfind ounit batteries yojson.1.3.2"

case "$OCAML_VERSION,$OPAM_VERSION" in
  3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
  3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
  3.12.1,1.2.0) ppa=avsm/ocaml312+opam12 ;;
  4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
  4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
  4.00.1,1.2.0) ppa=avsm/ocaml40+opam12 ;;
  4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
  4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
  4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
             *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

# -- Setup project dependencies

echo "yes" | sudo add-apt-repository ppa:$ppa
echo "yes" | sudo add-apt-repository ppa:george-edison55/cmake-3.x
sudo apt-get update -qq
sudo apt-get install --only-upgrade cmake
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMYES=1
opam --version
opam init 
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
opam update
opam install ${OPAM_DEPENDENCIES}
eval `opam config env`

# -- Build spirv-tools and inject path

mkdir ~/spirv-tools/build
cd ~/spirv-tools/build
cmake -G"Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ..
make
cd -
export PATH="$PATH:$HOME/spirv-tools/build/tools"

# -- Build and test project

make
make test
