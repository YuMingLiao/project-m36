{ compiler ? "ghc928"
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  needsCocoa = drv:
    if pkgs.stdenv.isDarwin
    then drv.overrideDerivation (old:
      { buildInputs = [ pkgs.darwin.apple_sdk.frameworks.Cocoa ] ++ old.buildInputs; }
    )
    else drv;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      curryer-rpc = self.callHackageDirect {
                      pkg = "curryer-rpc";
		      ver = "0.3.5";
		      sha256 = "sha256-7mEJOBKzA2rTnLxZme8E6zFv0VkiXBo5L/jUJSNPaNE="; } {};

      streamly = self.callHackageDirect {
                   pkg = "streamly";
		   ver = "0.9.0";
		   sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak="; } {};

      streamly-core = self.callHackageDirect {
      		    pkg = "streamly-core";
		    ver = "0.1.0";
  		    sha256 = "sha256-hoSV6Q2+X5a7hFnJAArqNPjcMaCVyX9Vz4FcxeJ+jgI="; } {};
		    
      streamly-bytestring = self.callHackageDirect {
                    pkg = "streamly-bytestring";
		    ver = "0.2.1";
  		    sha256 = "sha256-EcH6qq4nRjea3xQ66Zlqgjjg7lF/grkKJI0+tTO4B84="; } {};		    

      lockfree-queue = self.callHackageDirect {
      		     pkg = "lockfree-queue";
		     ver = "0.2.4";
                     sha256 = "sha256-h1s/tiBq5Gzl8FtenQacmxJp7zPJPnmZXtKDPvxTSa4="; } {};
      

      unicode-data = self.callHackageDirect {
                      pkg = "unicode-data";
		      ver = "0.2.0";
		      sha256 = "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
		     } {};

      winery = self.callHackageDirect {
                 pkg = "winery";
		 ver = "1.4";
                 sha256 = "sha256-ApJg6Qc25UyNZtSN52N9OrUQ/9K4w258oSE5BokO4tE=";
		 } {};

      barbies-th = self.callHackageDirect {
      	      pkg = "barbies-th";
	      ver = "0.1.10";
  	      sha256 = "sha256-cnTevB2qoEBMmGbqypQwJzPVF6z3cOXADbWF8OKQGAo=";	      
      } {};

      scotty = self.callHackageDirect {
      	     pkg = "scotty";
	     ver = "0.22";
             sha256 = "sha256-DY4lKmAmqGTrzKq93Mft9bu9Qc0QcsEVpKzgoWcBL2I=";
	     } {};

      wai = self.callHackageDirect {
            pkg = "wai";
	    ver = "3.2.4";
            sha256 = "sha256-NARmVhT5G1eMdtMM1xp7RFpevunThAB4tltCMih+qu8=";
	    } {};

      wai-extra = self.callHackageDirect {
           pkg = "wai-extra";
	   ver = "3.1.14";
           sha256 = "sha256-wMI9eTituRbMvYvbcA9pgIwFxkbdL1+2Xw78lghfWaU=";
	   } {};
    
      project-m36 = ((self.callCabal2nixWithOptions "project-m36" ./. "-f-haskell-scripting" {}));
    };
  };
in
{
  project = haskellPackages.project-m36;

  shell = haskellPackages.shellFor {
    packages = p: [
      p.project-m36
    ];
    buildInputs = [
      haskellPackages.ghcid
      haskellPackages.hlint
      pkgs.docker
    ];
    withHoogle = true;
  };
}
