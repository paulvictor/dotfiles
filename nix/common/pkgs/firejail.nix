self: super: {
  firejail = (super.firejail.overrideAttrs (oldAttrs: {
    patches = [
      (
      # Fix for https://github.com/netblue30/firejail/issues/2758
      super.writeText "fix-2758" ''
diff --git a/src/firejail/fs_hostname.c b/src/firejail/fs_hostname.c
index 8a3bb71e..a7d8682e 100644
--- a/src/firejail/fs_hostname.c
+++ b/src/firejail/fs_hostname.c
@@ -222,9 +222,6 @@ void fs_mount_hosts_file(void) {
 	struct stat s;
 	if (stat("/etc/hosts", &s) == -1)
 		goto errexit;
-	// not a link
-	if (is_link("/etc/hosts"))
-		goto errexit;
 	// owned by root
 	if (s.st_uid != 0)
 		goto errexit;
      ''
      )
    ];
  }));
}
