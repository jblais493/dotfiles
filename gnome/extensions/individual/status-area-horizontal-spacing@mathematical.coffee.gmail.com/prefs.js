import Adw from 'gi://Adw';
import Gtk from 'gi://Gtk';

import { ExtensionPreferences, gettext as _ } from 'resource:///org/gnome/Shell/Extensions/js/extensions/prefs.js';

export default class Prefs extends ExtensionPreferences {

    fillPreferencesWindow(window) {
        const settings = this.getSettings();

		const windowWidth = 750;
		const windowHeight = 580;
		const minValue = 0.0;
		const maxValue = 12.0;
		const increment = 1.0;
		const decimalDigits = 0;
		const key = "hpadding";
        
        const scale = new Gtk.Scale({
            digits: decimalDigits,
            adjustment: new Gtk.Adjustment({lower: minValue, upper: maxValue, step_increment: increment, page_increment: increment}),
            value_pos: Gtk.PositionType.RIGHT,
            hexpand: true, 
            halign: Gtk.Align.END
        });
        scale.set_draw_value(true);    
        scale.set_value(settings.get_int(key));
        scale.connect("value-changed", (sw) => {
            settings.set_int(key, sw.get_value());
        });
        scale.set_size_request(400, 15);

        const row = Adw.ActionRow.new();
        row.set_title(_("Horizontal Padding"));
        row.add_suffix(scale);

        const group = Adw.PreferencesGroup.new();
        group.add(row);

        const page = Adw.PreferencesPage.new();
        page.add(group);

        window.set_default_size(windowWidth, windowHeight);
        window.add(page);
		window.set_focus(scale);
    }

}
