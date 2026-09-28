{ writeShellApplication }:
writeShellApplication {
  name = "marlowe-indexer";
  text = ''
    mkdir -p /tmp
    {
      echo "argv: $*"
      echo "user: $(id -un)"
    } > /tmp/marlowe-indexer.started
    exec sleep infinity
  '';
}
