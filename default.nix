{ mkDerivation, acid-state, attoparsec, base, blaze-html
, bytestring, clckwrks, containers, directory, filepath
, happstack-hsp, happstack-server, hsp, hsx2hs, ircbot, ixset, mtl
, network, reform, reform-happstack, reform-hsp, safecopy, stdenv
, text, web-plugins, web-routes, web-routes-th
}:
mkDerivation {
  pname = "clckwrks-plugin-ircbot";
  version = "0.6.15";
  src = ./.;
  libraryHaskellDepends = [
    acid-state attoparsec base blaze-html bytestring clckwrks
    containers directory filepath happstack-hsp happstack-server hsp
    ircbot ixset mtl network reform reform-happstack reform-hsp
    safecopy text web-plugins web-routes web-routes-th
  ];
  libraryToolDepends = [ hsx2hs ];
  homepage = "http://clckwrks.com/";
  description = "ircbot plugin for clckwrks";
  license = stdenv.lib.licenses.bsd3;
}
