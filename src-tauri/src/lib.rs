use tauri::{LogicalSize, Manager, Size};

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_store::Builder::new().build())
        .plugin(tauri_plugin_opener::init())
        .setup(|app| {
            #[cfg(debug_assertions)]
            {
                // Opens the DevTools on launch in dev.
                // let window = app.get_webview_window("main").unwrap();
                // window.open_devtools();

                #[cfg(any(target_os = "windows", target_os = "macos", target_os = "linux"))]
                {
                    let window = app.get_webview_window("main").unwrap();
                    window
                        .set_size(Size::Logical(LogicalSize {
                            width: 300.0,
                            height: 500.0,
                        }))
                        .unwrap();
                }
            }
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
