from os import listdir, path
import pickle as _pickle
import re
import json
import cairosvg
import eyekit

def pickle(obj, file_path):
	with open(file_path, mode='wb') as file:
		_pickle.dump(obj, file)

def unpickle(file_path):
	with open(file_path, mode='rb') as file:
		return _pickle.load(file)

def load_passages(passages_dir):
	passages = {}
	for passage_file in listdir(passages_dir):
		if not passage_file.endswith('.txt'):
			continue
		passage_id, _ = passage_file.split('.')
		passage_path = path.join(passages_dir, passage_file)
		passages[passage_id] = eyekit.Passage(passage_path,
			first_character_position=(368, 155),
			character_spacing=16,
			line_spacing=64,
			pad_lines_with_spaces=True)
	return passages

def load_data(data_file):
	with open(data_file) as file:
		data = json.load(file)
	return data

def convert_svg(svg_file_path, out_file_path, png_width=1000):
	filename, extension = path.splitext(out_file_path)
	if extension == '.pdf':
		cairosvg.svg2pdf(url=svg_file_path, write_to=out_file_path)
	elif extension == '.eps':
		cairosvg.svg2ps(url=svg_file_path, write_to=out_file_path)
	elif extension == '.png':
		cairosvg.svg2png(url=svg_file_path, write_to=out_file_path, dpi=300)
	else:
		raise ValueError('Cannot save to this format. Use either .pdf, .eps, or .png')

def format_svg_labels(svg_file_path, monospace=[], arbitrary_replacements={}):
	'''
	Applies some nicer formatting to an SVG plot, including setting
	the font to Helvetica and adding italics. Requires you to set
	this at the top of the script:
	plt.rcParams['svg.fonttype'] = 'none'
	'''
	with open(svg_file_path, mode='r', encoding='utf-8') as file:
		svg = file.read()
	svg = re.sub(r'font-family:.*?;', 'font-family:Helvetica Neue;', svg)
	for check in re.finditer('<text.*?✔</text>', svg):
		if check:
			svg = svg.replace(check.group(0), check.group(0).replace('Helvetica Neue', 'Menlo'))
	for word in monospace:
		for matched_line in re.finditer('<text.*?%s</text>'%word, svg):
			if matched_line:
				svg = svg.replace(matched_line.group(0), matched_line.group(0).replace('Helvetica Neue', 'Menlo'))
	for find, replace in arbitrary_replacements.items():
		svg = svg.replace(find, replace)
	with open(svg_file_path, mode='w', encoding='utf-8') as file:
		file.write(svg)
