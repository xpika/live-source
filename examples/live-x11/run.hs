import System.Cmd
 
 
main = do
        x <- system "export LD_LIBRARY_PATH=/opt/local/lib; ./example"
        return ()

